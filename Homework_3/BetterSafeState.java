import java.util.concurrent.locks.ReentrantLock;
import java.util.Set;
import java.util.HashSet;

class BetterSafeState implements State {
    private byte[] value;
    private ReentrantLock[] locks;
    private byte maxval;

    BetterSafeState(byte[] v) {
	value = v;
	locks = new ReentrantLock[value.length];
	for(int i=0; i < value.length; ++i)
	    locks[i] = new ReentrantLock();
        maxval = 127;
    }

    BetterSafeState(byte[] v, byte m) {
	value = v;
	locks = new ReentrantLock[value.length];
	for(int i=0; i < value.length; ++i)
	    locks[i] = new ReentrantLock();
	maxval = m;	
    }

    public int size() { return value.length; }

    public byte[] current() {
        return value;
    }

    public boolean swap(int i, int j) {
       // start critical section 
       // lock before the check of <= 0 and >= maxval
       // release lock before returning false. If you make
       // it through this if condition, then release lock 

	if(locks[i].tryLock()) {
	    if(locks[j].tryLock()) {
		if(value[i] <= 0 || value[j] >= maxval) {
			locks[i].unlock();
			locks[j].unlock();
			return false;
		}

		value[i]--;
		value[j]++;
		locks[i].unlock();
		locks[j].unlock();
		return true;
	    } 
	
	    locks[i].unlock();
	}

	return false;

    }
}
