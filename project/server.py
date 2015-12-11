from twisted.internet import reactor
from twisted.internet import protocol
from twisted.protocols.basic import LineReceiver
from twisted.python import log
from twisted.web.client import getPage
from twisted.application import service
from twisted.application import internet

import json
import datetime
import logging
import time
import re
import sys

GOOG_PLACES_API_KEY = 'AIzaSyBApheFbjjD5xwixlpP_zC6FmqjvpHkPrk'
GOOG_PLACES_API_URL = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?location='

PORTS = {
    "Alford" : 13000,
    "Bolden" : 13001,
    "Hamilton" : 13002,
    "Parker" : 13003,
    "Powell" : 13004
}

HERD = {
	"Alford"	: ["Parker", "Powell"],
	"Bolden"	: ["Parker", "Powell"],
	"Hamilton"	: ["Parker"],
	"Parker"	: ["Alford", "Bolden", "Hamilton"],
	"Powell"	: ["Alford", "Bolden"]
}

class MyProtocol(LineReceiver):
	def __init__(self, factory):
		self.factory = factory

	def lineReceived(self, line):
		print "Line Received"
		logging.info('Line Received: {0}'.format(line))
		lineWords = line.split()
		
		if(lineWords[0] == "IAMAT"):
			self.doIAMAT(line)
		elif(lineWords[0] == "WHATSAT"):
			self.doWHATSAT(line)
		elif(lineWords[0] == "AT"):
			self.doAT(line)
		else:
			logging.info('Invalid Line: {0}'.format(line))
			self.transport.write('? {0}\n'.format(line))
		return
		
	'''
	IAMAT Format (4 words separated by whitespace): 
		- Command Name
		- Client ID
		- LatLng
		- Time
	'''
	def doIAMAT(self, line):
		print "doing IAMAT"
		lineWords = line.split()
		if(len(lineWords) != 4):
			logging.error('Invalid IAMAT: {0}'.format(line))
			self.transport.write("? {0}\n".format(line))
			return
		
		id = lineWords[1]
		pos = lineWords[2]
		timeLapsed = time.time() - float(lineWords[3])
		
		# create response for + or - depending on clock skew
		if int(timeLapsed) >= 0:
			response = "AT {0} +{1} {2} {3} {4}".format(self.factory.name, timeLapsed, id, pos, lineWords[3])
		else:
			response = "AT {0} {1} {2} {3} {4}".format(self.factory.name, timeLapsed, id, pos, lineWords[3])
			
		if id in self.factory.clients:
			logging.info("Known client: {0}".format(id))
		else:
			logging.info("New client: {0}".format(id))
		
		logging.info("Response to client: {0}".format(response))
		self.transport.write("{0}\n".format(response))
		
		posSplit = pos.split('+')[1].split('-')
		self.factory.clients = {"lat": posSplit[0], "long": posSplit[1], "timestamp": lineWords[3], "at": response}
		
		logging.info("Updating neighbors")
		self.updateHerd(response)
		return
			
	'''
	WHATSAT Format (4 words separated by whitespace):
		- Command Name
		- client ID
		- Radius in KM
		- Number of results desired
	'''
	def doWHATSAT(self, line):
		print "doing WHATSAT"
		lineWords = line.split()
		if (len(lineWords) != 4):
			logging.error('Invalid WHATSAT: {0}'.format(line))
			self.transport.write('? {0}\n'.format(line))
			return
		
		id = lineWords[1]
		r = lineWords[2]
		
		if(int(r) > 50):
			logging.error('Invalid WHATSAT: Radius is {0}'.format(r))
			self.transport.write('? {0}\n'.format(line))
			return
		
		if (not (id in self.factory.clients)):
			logging.error('Invalid WHATSAT: Client {0} not found'.format(id))
			self.transport.write('? {0}\n'.format(line))
			return
		
		clientAT = self.factory.clients[id]["response"]
		
		clientInfo = self.factory.clients[id]
		lat = clientInfo["lat"]
		long = clientInfo["long"]
		pos = clientAT.split()[4]
		logging.info("Gathering Client {0} Position: {1}".format(id, pos))
		
		posQuery = pos.replace('+', ' +').replace('-', ' -').strip().replace(' ', ',')
		
		url = "{0}{1}&radius={2}&sensor=false&key={3}".format(GOOG_PLACES_API_URL, posQuery, r, GOOG_PLACES_API_KEY)
		logging.info("API Request: {0}".format(url))
		response = getPage(url)
		response.addCallback(lambda x: self.sendResults(x, id))
		return
	
	'''
	AT Format (6 words separated by whitespace):
		- Command Name
		- Server ID
		- Time Lapsed between client time and receipt
		- client ID
		- LatLng
		- Time server sent message
	'''
	def doAT(self, line):
		print "doing AT"
		lineWords = line.split()
		if(len(lineWords) != 6):
			logging.error("Invalid AT: {0}".format(line))
			self.transport.write("? {0}\n".format(line))
			return
		
		name = lineWords[1]
		id = lineWords[3]
		pos = lineWords[4]
		time = lineWords[5]
		
		if(id in self.factory.clients):
			if(time > self.factory.clients[id]["timestamp"]):
				logging.info("Location Update from: {0}".format(id))
			else:
				logging.info("Ignoring update from: {0}".format(id))
				return
		else:
			logging.info("New Client: {0}".format(id))
			
		posSplit = pos.split('+')[1].split('-')
		lat = posSplit[0]
		long = posSplit[1]
		self.factory.clients[id] = {"lat": lat, "long": long, "timestamp": time, "response": line}
		logging.info("Client {0} Location Saved".format(id))
		
		#flood update neighbours
		self.updateHerd(line)
		return
		
	def connectionMade(self):
		print "Connection Made"
		logging.info('{0} is connected to a new client'.format(self.factory.name))
		#self.factory.clients.append(self)
		self.factory.numConnections += 1
		return

	def connectionLost(self, reason):
		print "Connection Lost"
		logging.info(' {0} has disconnected from a client'.format(self.factory.name))
		#self.factory.clients.remove(self)
		self.factory.numConnections -= 1
		return
	
	#connect and update herd by flooding
	def updateHerd(self, message):
		for s in HERD[self.factory.name]:
			reactor.connectTCP("localhost", PORTS[s], MyLineSender(message))
			logging.info("Sending {0} location to {1}".format(self.factory.name, s))
		return
	
	def jsonSend(self, id):
		data = json.loads(response)
		jsonData = json.dumps(data, indent=4)
		
		atResponse = self.factory.clients[id]["response"]
		response = "{0}\n{1}\n\n".format(atResponse, json_data)
		logging.info("Sending WHATSAT: {0}".format(response))
		self.transport.write(response)
		
class MyServerFactory(protocol.ServerFactory):
	def __init__(self, name):
		self.clients = {}
		self.name = name
		self.portNum = HERD[name]
		self.numConnections = 0

		logfile = name + ".log"
		logging.basicConfig(filename = logfile, level = logging.DEBUG, filemode = 'a', format='%(asctime)s %(message)s')
		logging.info('{0} Server Set Up'.format(self.name))
		
	def buildProtocol(self, addr):
		return MyProtocol(self)

class MyLineSenderProtocol(LineReceiver):
	def __init__(self, factory):
		self.factory = factory
	
	def connectionMade(self):
		self.sendLine(self.factory.message)
		self.transport.loseConnection()
		
class MyLineSender(protocol.ClientFactory):
	def __init__(self, message):
		self.message = message
	
	def buildProtocol(self, addr):
		return MyLineSenderProtocol(self)

def main():
	if len(sys.argv) != 2:
		print "Error: Incorrect Number of Arguments"
		exit()

	name = sys.argv[1]
	if name in HERD:
			reactor.listenTCP(PORTS[name], MyServerFactory(name))
			reactor.run()
	else:
		print "Error: Invalid Server Name"
		exit()
		

if(__name__ == "__main__"):
	main()

