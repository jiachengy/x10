import x10.util.Timer;
import x10.util.ArrayList;
import x10.util.HashMap;
import x10.util.Pair;
import x10.util.concurrent.*;

/**
 * This is the class that provides the HashMap functionalities.
 *
 * The assignment is to replace the content of this class with code that exhibit
 * a better scalability.
 */

public class Hash
{
    // the default capacity of the hash table
    // the maximum number of entries we can hold in the hash table
    private val capacity = 1 << 24;
    private val mask = capacity - 1;

	private var keys : Rail[AtomicLong];
    private var values : Rail[long];
    private var mutex : Rail[AtomicBoolean];

	private var counter : AtomicLong;
	private var defaultValue : Long; // a default value is returned when an element with a given key is not present in the dict.

    static val INVALID_KEY = -1; // indicate a pending operations
    
    // utility function to compute the next power of 2
    // for calculating hash table capacity
    static def next_pow_2(value : long) : long {
        var v : long = value;
        v--;   
        v = (v >> 1) | v;
        v = (v >> 2) | v;
        v = (v >> 4) | v;
        v = (v >> 8) | v;
        v = (v >> 16) | v;
        v++;
        return v;
    }


	public def this(defV : long){
        // a global atomic counter
		counter = new AtomicLong();
        
        // initialize the buckets
        keys = new Rail[AtomicLong](capacity);
        values = new Rail[long](capacity);
        mutex = new Rail[AtomicBoolean](capacity);

        // initialize atomic variables
        for (i in 0..(capacity-1)) {
            keys(i) = new AtomicLong(INVALID_KEY);
            mutex(i) = new AtomicBoolean();
        }
		defaultValue = defV;
	}

    /**
     * Insert the pair <key,value> in the hash table
     *     'key'
     *     'value' 
     *
     * This function return the unique order id of the operation in the linearized history.
     */
    public def put(key: long, value: long) : long
    {
		var r : long = -1;

        for (var idx:long = key.hashCode(); ; idx++) {
            idx &= mask;

            // read existing key
            val exkey = keys(idx).get();
            if (exkey != key) {
                 // the slot is not mine
                 if (exkey != INVALID_KEY)
                    continue;
                 
                 // try take the slot
                 if (!keys(idx).compareAndSet(INVALID_KEY, key))
                    if (keys(idx).get() != key)
                       continue;
            }        
            // found my slot
            // wait until my turn
            while (!mutex(idx).compareAndSet(false, true));
            values(idx) = value;
            r = counter.getAndIncrement();
            mutex(idx).set(false);
            return r;
        }                        
    }

    /**
     * get the value associated to the input key
     *     'key'
     *
     * This function return the pair composed by
	 *     'first'    unique order id of the operation in the linearized history.
	 *     'second'   values associated to the input pair (defaultValue if there is no value associated to the input key)
     */
    public def get(key: long) : Pair[long,long]
    {
		var r:long = -1;
        var value:long = defaultValue;

        // read has a similar logic as the write
        // because we also want a linearlization point for reader
        // if reads do not need to be serializable, we can simplify
        // the logic below to make it much faster

        for (var idx:long = key.hashCode(); ; idx++) {
            idx &= mask;

            // read existing key
            val exkey = keys(idx).get();
            if (exkey != key) {
                 // the slot is not mine
                 if (exkey != INVALID_KEY)
                    continue;
                 
                 // try take the slot
                 if (!keys(idx).compareAndSet(INVALID_KEY, key))
                    if (keys(idx).get() != key)
                        continue;
            }        
            // found my slot
            // wait until my turn
            while (!mutex(idx).compareAndSet(false, true));
            // read whatever in the current slot, it may be empty
            value = values(idx);
            r = counter.getAndIncrement();
            mutex(idx).set(false);
            return new Pair[long,long](r, value);
        }                        
     }        


}