import x10.io.Console;
import x10.util.Timer;
import x10.util.ArrayList;
import x10.array.Array_2;

/**
 * This is the class that provides the solve() method.
 *
 * The assignment is to replace the contents of the solve() method
 * below with code that actually works :-)
 */
public class Solver
{
    /* Configurations */
    var N:Long;
    var pawns:ArrayList[Point{rank==2}];
    // how many partititions to use in the non-pawn case
    val P = 8;
    
    /* (N*N)*(N*N) visibility matrix */
    /* visible(i, j) indicates whether queen at i is visible to queen at j */
    /* the index is converted to a linear address space */
    var visible:Array_2[Boolean];

    /* Indicate whether a location is taken by the pawn */
    var occupied:Rail[Boolean];

    /* Indicate whether we can prune this row */
    /* if there is no pawn on the way, the row can be pruned */
    var prune:Rail[Boolean];    

    /* maximum number of queens able to placed at and after this row */
    var atmost:Rail[Long];    

    /* Solver for non-pawn games */
    def start() {
        new Board().search();
    }

    /* Solver for games with pawn */
    def start_with_pawn() {
        build_visibility();
        new PawnBoard().search(0);
    }


    /*
     * Partition an array into P blocks
     */
    public static def block(R: LongRange, P: Long): Rail[LongRange] = {
    	assert P >= 0;
    	val low = R.min, high = R.max, count = high-low+1n;
    	val baseSize = count/P, extra = count - baseSize*P;
    	new Rail[LongRange](P, (i:Long):LongRange => {
    		val start = low+i*baseSize+ (i < extra? i:extra);
    		start..(start+baseSize+(i < extra?0:-1))
    	})
    }

    /* NQueens Board */
    class Board {
        val q: Rail[Long];
        def this() {
            q = new Rail[Long](0, (Long)=>0);
        }
        def this(old: Rail[Long], newItem:Long) {
            val n = old.size;
            q = new Rail[Long](n+1, (i:Long)=> (i < n? old(i) : newItem));
        }
        def safe(j: Long) {
            val n = q.size;
            for (k in 0..(n-1)) {
                if (j == q(k) || Math.abs(n-k) == Math.abs(j-q(k)))
                    return false;
            }
            return true;
        }

        /** Search for all solutions in parallel.
         */
        def search(range:LongRange) {
            for (k in range)
                if (safe(k)) async new Board(q, k).search();
        }

        def search()  offers Int {
            if (q.size == N) {
               offer 1n;
               return;
            }

            if (q.size == 0) {
                val R = block(0..(N-1), P);
                for(q in 0..(P-1)) async
                  search(R(q));
            } else search(0..(N-1));
        }
    }

    /* NQueens with Pawn Board */
    class PawnBoard extends Board {
        def this() {
            super();
        }
        def this(old: Rail[Long], newItem:Long) {
            super(old, newItem);
        }
        def safe(j: Long) {
            /* is there is pawn at this position */
            if (occupied(j))
               return false;
            val n = q.size;
            for (k in 0..(n-1)) {
                if (visible(j, q(k)) == true)
                    return false;
            }
            return true;
        }

        /** Search for all solutions in parallel.
         */
        def search(range:LongRange) {
            for (k in range) {
                // more pruning?
                if (safe(k)) {
                   val row = k / N;
                   if (prune(row))
                      async new PawnBoard(q, k).search((row+1) * N);
                   else
                      async new PawnBoard(q, k).search(k+1);
                }
            }
        }

        /* search after the item */
        def search(begin:Long)  offers Int {
            if (q.size == N) offer 1n;
            else {
                 val row = begin / N;
                 // how many more do you need to place?
                 val more = N - q.size;
                 var r:Long;
                 r = N-1;
                 while (r >= row && atmost(r) < more)
                     r--;
                 // at the end of loop
                 // you can not place the next queen after that r
                 search(begin..(N*(r+1)-1));
            }
        }
    }


    static val sum = new Reducible[Int]() {
    	public def zero()=0n;
    	public operator this(i:Int,j:Int)=i+j;
    };

    def check_occupied() {
        for (idx in 0..(N*N-1))
            Console.OUT.println(idx + " " + occupied(idx));
    }

    def check_visibility() {
    }
    
    /* Build a visibility matrix */
    def build_visibility(){
        /* mark pairs that are blocked */
        occupied = new Rail[Boolean](N*N, (Long)=>false);
        visible = new Array_2[Boolean](N*N, N*N, (Long, Long)=>false);
        
        // initially, assuming an empty board
        // every row can place only 1 queen
        atmost = new Rail[Long](N);
        for (i in 0..(N-1))
            atmost(i) = 1;

        // set visibility
        // for each point, find out all points
        // reachable by it
        finish {
        for (i1 in 0..(N*N-1)) {
            async {
            val row = i1 / N;
            val col = i1 % N;

            // set row
            for (r in 0..(N-1)) {
                val i2 = r * N + col;
                visible(i1, i2) = true;
            }

            // set col
            for (c in 0..(N-1)) {
                val i2 = row * N + c;
                visible(i1, i2) = true;
            }
            
            // set diagonal
            for (k in (-Math.min(row,col))..Math.min(N-row-1, N-col-1)) {
                val r2 = row + k;
                val c2 = col + k;
                val i2 = r2 * N + c2;
                visible(i1, i2) = true;
            }

            // set anti-diagnoal
            for (k in (-Math.min(row,N-col-1))..Math.min(N-row-1, col)) {
                val r2 = row + k;
                val c2 = col - k;
                val i2 = r2 * N + c2;
                visible(i1, i2) = true;
            }
            }
        }
        }
        
        finish {
        for (pawn in pawns) {
            async {
            val row = pawn(0);
            val col = pawn(1);
            // compute linear address
            val idx = row * N + col;
            occupied(idx) = true;

            // we can not prune this row
            atomic prune(row) = false;
            // increase the number of queens we can place on this row by 1
            atomic atmost(row) += 1;
            
            // cut off row
            for (c1 in 0..(col-1)) {
                for (c2 in (col+1)..(N-1)) {
                    val i1 = row * N + c1;
                    val i2 = row * N + c2;
                    visible(i1, i2) = false;
                    visible(i2, i1) = false;
                }
            } 

            // cut off col
            for (r1 in 0..(row-1)) {
                for (r2 in (row+1)..(N-1)) {
                    val i1 = r1 * N + col;
                    val i2 = r2 * N + col;
                    visible(i1, i2) = false;
                    visible(i2, i1) = false;
                }
            } 

            // cut off diagonal
            for (k1 in 1..Math.min(row, N-col-1)) {
                for (k2 in 1..Math.min(N-row-1, col)) {
                    val r1 = row - k1; val c1 = col + k1;
                    val r2 = row + k2; val c2 = col - k2;
                    val i1 = r1 * N + c1;
                    val i2 = r2 * N + c2;
                    visible(i1, i2) = false;
                    visible(i2, i1) = false;
                }                   
            }
                    

            // cut off anti-diagonal
            for (k1 in 1..Math.min(row, col)) {
                for (k2 in 1..Math.min(N-row-1, N-col-1)) {
                    val r1 = row - k1; val c1 = col - k1;
                    val r2 = row + k2; val c2 = col + k2;
                    val i1 = r1 * N + c1;
                    val i2 = r2 * N + c2;
                    visible(i1, i2) = false;
                    visible(i2, i1) = false;
                }                   
            }
            } // end sync
        }
        } // end finish

        // minimum of number of empty spaces and number of intervals
        for (i in 0..(N-1))
            atmost(i) = Math.min(atmost(i), N-atmost(i)+1);

        // compute prefix sum over the atmost
        for (var i:Long = N-2; i >=0; i--)
            atmost(i) = atmost(i+1) + atmost(i);
    }



    /**
     * Solve a single 'N'-Queens with pawns problem.
     *     'size' is 'N'.
     *     'pawns' is an array of bidimensional Point {rank==2} with the locations of pawns.  The array may be of length zero.
     *
     * This function should return the number of solutions for the given configuration.
     */
    public def solve(size: int, pawns: ArrayList[Point{rank==2}]) : int
    {
        if (size == 0n)
           return 0n;

        N = size;
        this.pawns = pawns;

        // initially, all rows can be pruned
        prune = new Rail[Boolean](N, (Long)=>true);

        // Handle 0-pawn case separtely
        if (pawns.size() == 0)
        {        
            val count = finish(sum){start();};
            return count; 
        }
        else
        {
           val count = finish(sum){start_with_pawn();};
           return count;
        }
    }

}