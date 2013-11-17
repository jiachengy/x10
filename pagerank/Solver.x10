import x10.util.Timer;
import x10.util.ArrayList;
import x10.regionarray.*;

/**
 * This is the class that provides the solve() method.
 *
 * The assignment is to replace the contents of the solve() method
 * below with code that actually works :-)
 */

class Node {
    val id: Int;
    val links: Rail[Int];
    def this(id:Int, links:Rail[Int]) {
        this.id = id;
        this.links = links;
    }

    def toString() {
    	var result:String = id + " : ";
    	for(var i:Int = 0n; i<links.size; i++) {
    		result += links(i)+" ";
    	}
    	return result;
    }
}

public class Solver
{
    static val ITERATIONS = 1000000;
    static val NTHREADS = x10.lang.Runtime.NTHREADS * 2; // spawn at most twice as many threads

    /*
     * Flatten the adjacency list in WebNode to Integer Array
     * so that it won't cause recursive copy of the WebNode object
     * during remote access
     */
    def convertWebGraph(webGraph : Rail[WebNode]) : Rail[Node] {
        var graph:Rail[Node] = new Rail[Node](webGraph.size);
        finish {
            for (t in 0..(NTHREADS-1)) {
                val begin = t;
                async {
                    for (var i:Long = begin; i < webGraph.size; i+=NTHREADS) {
                        val page = webGraph(i);
                        val links = new Rail[Int](page.links.size());
                        for (j in 0..(links.size-1))
                            links(j) = page.links(j).id;
                        val n = new Node(page.id, links);
                        graph(i) = n;
                    }
                }
            }
        }
        
        return graph;
    }


    /**
     * solve(webGraph: Rail[WebNode], dampingFactor: double,epsilon:Double)
     * 
     * Returns an approximation of the page rank of the given web graph
     */
    public def solve(webGraph: Rail[WebNode], dampingFactor: Double, epsilon:Double) : Rail[Double] {
        val n = webGraph.size;

        val webg = convertWebGraph(webGraph);

        // build the Q matrix
        val start = Timer.milliTime();
        val Q = buildMatrix(webg, dampingFactor);
        val end = Timer.milliTime();
        //        Console.OUT.println("page rank time: " + (end-start));

        // run page rank
        val start2 = Timer.milliTime();
        val solutions = pagerank(Q, n, epsilon);
        val end2 = Timer.milliTime();
        //        Console.OUT.println("page rank time: " + (end2-start2));

        return solutions;
    }


    /*
     *  Run the page rank algorithm on the Q matrix
     */
    public def pagerank(Q:DistArray[Double], n:Long, epsilon:Double) : Rail[Double] {
        // val local_p = PlaceLocalHandle.make[Rail[Double]](PlaceGroup.WORLD, ()=>new Rail[Double](n));
        val local_p = DistArray.make[Double](Dist.makeBlock(Region.make(0..(n-1))));
        val local_new_p = PlaceLocalHandle.make[Rail[Double]](PlaceGroup.WORLD, ()=>new Rail[Double](n));
        // aggregated p
        val global_p = new Rail[Double](n, (i:Long)=>1.0/n);
        val old_global_p = new Rail[Double](n);
        
        for (iter in 1..ITERATIONS) {
            // update local p
            finish for (d in Place.places()) {
                at (d) async {
                    val region = local_p.dist(here);
                    val mincol = region.min(0);
                    val maxcol = region.max(0);
                        
                    // copy global to local
                    for (var j:Long = mincol; j <= maxcol; ++j)
                        local_p(j) = global_p(j);
                            
                    // reset local_new
                    for (j in 0..(n-1))
                        local_new_p()(j) = 0.0;
                }
            }
            
            // compute local p for local columns
            finish for (d in Place.places()) {
                at (d) async {
                    val region = Q.dist(here);
                    // the starting column of place d
                    val mincol = region.min(1);
                    // the last column of place d
                    val maxcol = region.max(1);
                           
                    finish for (t in 0..(NTHREADS-1)) {
                        val idx = t;
                        async {
                            for (var i:Long = idx; i < n; i+=NTHREADS) {
                                var newv : Double = 0;
                                for (var j:Long = mincol; j <= maxcol; ++j) {
                                    newv += local_p(j) * Q(i, j);
                                }
                                local_new_p()(i) = newv;
                            }                            
                        }
                    }
                }
            }

            // copy previous global p to old
            for (j in 0..(n-1)) {
                old_global_p(j) = global_p(j);
                global_p(j) = 0.0;
            }
            
            // aggreagate
            finish {
                val global_p_gr = GlobalRef(global_p);
                val there = here;
                for (d in Place.places()) at(d) async {
                    val temp_local_p = local_new_p();
                    at (there) atomic {
                        for (j in 0..(n-1))
                            global_p_gr()(j) += temp_local_p(j);
                    }
                       
                }
            }

            // normalize the vector
            var len:Double = 0;
            for (j in 0..(n-1))
                len += global_p(j);

            for (j in 0..(n-1))
                global_p(j) /= len;

            
            // Test for convergence
            var b:Boolean = true;
            for (j in 0..(n-1)) { 
                if (Math.abs(old_global_p(j)-global_p(j)) > epsilon) {
                    b = false;
                    break;
                }
            }
            if (b) break;
        }
        
        return global_p;
    }


     
    
    /*
     * Construct the Q matrix in multiple places in parallel
     * Input:
     *    - webGraph : the transformed webGraph
     *    - dampingFactor
     * Output:
     *    - the Q matrix in a distributed 2-D array
     * 
     */
    public def buildMatrix(webGraph : Rail[Node],
                           dampingFactor : Double) : DistArray[Double] {
        // number of pages
        val n = webGraph.size;
        // N * N matrix region
        val mat_region = Region.make(0..(n-1), 0..(n-1));
        // partition the region by column
        val mat_dist = Dist.makeBlock(mat_region, 1);
        // Q matrix - distributed
        val Q = DistArray.make[Double](mat_dist, 0);

        val web_region = Region.make(0..(n-1));
        val web_dist = Dist.makeBlock(web_region);
        val webGraphD = DistArray.make[Node](web_dist);

        // dispatch the web graph adjacency list to places
        finish for (t in 0..(NTHREADS-1)) {
            val begin = t;
            async {
                for (var i:Long = begin; i < n; i+=NTHREADS) {
                    val page = webGraph(i);
                    val idx = i;
                    at (web_dist(i))
                        webGraphD(idx) = page;
                }
            }
        }

        // verify dispatch
        // for (p in 0..(n-1)) {
        //     at (web_dist(p)) {
        //         Console.OUT.println("[" + here.id + "]" + webGraphD(p));
        //     }
        // }
        
        // now let each place build the matrix in parallel
        finish {
            for (d in mat_dist.places()) at (d) {
                // store the damping factor locally
                val df = dampingFactor;
                // get the region of place d
                val region = mat_dist(here);
                // the starting column of place d
                val mincol = region.min(1);
                // the last column of place d
                val maxcol = region.max(1);
                
                // spawn NTHREADS async
                finish {
                    for (t in 0..(NTHREADS-1)) {
                        val begin = mincol + t;
                        async {
                            for (var i:Long = begin; i <= maxcol; i+=NTHREADS) {
                                // construct column i for each dimension
                                for (var j:Long = 0; j < n; ++j) {
                                    Q(j, i) = (1.0 - df) / n;
                                }
                                if (webGraphD(i).links.size == 0) {
                                    val prob = 1.0 / n * df;
                                    for (link in 0..(n-1))
                                        Q(link, i) += prob;
                                }
                                else {
                                    val prob = 1.0 / webGraphD(i).links.size * df;
                                    for (link in webGraphD(i).links)
                                        Q(link-1, i) += prob;
                                }
                            }
                        }
                    }
                }
            }
        }

        // print the matrix
        // finish {
        // for (var col:Long = 0; col < n; ++col) {
        //     val c = col;
        //     at (mat_dist(0,col)) {
        //         Console.OUT.println("["+here.id+"]");
        //         var result:String = "";
        //         for (i in 0..(n-1))
        //             result = result + Q(i,c) + " ";
        //         Console.OUT.println(result);
        //     }
        // }
        // }

        return Q;
    }
}
