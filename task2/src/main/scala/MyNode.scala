// Robin Jean-Sébastien Yvon Debalme
package task2

import scala.collection.mutable.Queue
import scala.collection.mutable.Set
import scala.util.control.Breaks._
import scala.collection.mutable.Queue

class MyNode(id: String, memory: Int, neighbours: Vector[String], router: Router) extends Node(id, memory, neighbours, router) {
    val STORE = "STORE"
    val STORE_SUCCESS = "STORE_SUCCESS"
    val STORE_FAILURE = "STORE_FAILURE"
    val RETRIEVE = "RETRIEVE"
    val GET_NEIGHBOURS = "GET_NEIGHBOURS"
    val NEIGHBOURS_RESPONSE = "NEIGHBOURS_RESPONSE"
    val RETRIEVE_SUCCESS = "RETRIEVE_SUCCESS"
    val RETRIEVE_FAILURE = "RETRIEVE_FAILURE"
    val INTERNAL_ERROR = "INTERNAL_ERROR"
    val USER = "USER"
    val nodes_may_fail = 4

    val GET_BOOLMEMORY = "GET_BOOLMEMORY"
    val BOOLMEMORY = "BOOLMEMORY"

    override def onReceive(from: String, message: Message): Message = {
        /* 
         * Called when the node receives a message from some where
         * Feel free to add more methods and code for processing more kinds of messages
         * NOTE: Remember that HOST must still comply with the specifications of USER messages
         *
         * Parameters
         * ----------
         * from: id of node from where the message arrived
         * message: the message
         *           (Process this message and decide what to do)
         *           (All communication between peers happens via these messages)
         */
        if (message.messageType == GET_NEIGHBOURS) { // Request to get the list of neighbours
            new Message(id, NEIGHBOURS_RESPONSE, neighbours.mkString(" "))
        }
        else if (message.messageType == RETRIEVE) { // Request to get the value
            val key = message.data // This is the key
            val value = getKey(key) // Check if the key is present on the node
            var response : Message = new Message("", "", "")
            value match {
                case Some(i) => response = new Message(id, RETRIEVE_SUCCESS, i)
                case None => response = {
                    if (from == USER) retrievalBfs(id, message, Queue[String](id) , Map[String, String]((id, "visited")), new Message(id, RETRIEVE_FAILURE))//retrieval2(id, neighbours, router, message, Map[String, String](id -> "checked"), Map[String, String]())
                    else new Message(id, RETRIEVE_FAILURE)
                }
            }
            /*
             * TODO: task 2.1
             * Add retrieval algorithm to retrieve from the peers here
             * when the key isn't available on the HOST node.
             * Use router.sendMessage(from, to, message) to send a message to another node
             */
            
            /*def retrieval(node: String, neighbours: Vector[String], router: Router, message: Message, checked: Vector[String], through: Vector[String]): Message = {
                
                val messFromNeighbours = neighbours.map(n => router.sendMessage(node, n, message))
                val anySucess = messFromNeighbours.filter(x => x.messageType == RETRIEVE_SUCCESS)
                if (!(anySucess.isEmpty))
                    anySucess.head
                else {
                    val anySucess2 = neighbours.map(n =>{
                        if (through.contains(n)) new Message(node, RETRIEVE_FAILURE)
                        else {
                            val neighFromN = router.sendMessage(node, n, new Message(node, GET_NEIGHBOURS)).data.split(" ").toVector
                                    //println(neighFromN)
                            retrieval(n, neighFromN, router, message, (checked).union(neighbours).distinct, through :+ node)
                        }
                    }).filter(x => x.messageType == RETRIEVE_SUCCESS)
                    if (!(anySucess2.isEmpty))
                       anySucess2.head
                    else new Message(node, RETRIEVE_FAILURE)
                        }
                    }*/

            /*def retrieveClose(node: String, neighbours: Vector[String], message: Message): Message = neighbours.isEmpty match {
                case true => new Message(node, RETRIEVE_FAILURE)
                case _ => {
                    val n = neighbours.head
                    val tmp = router.sendMessage(node, n, message)
                    if (tmp.messageType == RETRIEVE_SUCCESS)
                        tmp
                    else retrieveClose(node, neighbours.tail, message)
                }
            }

            def retrieval2(node: String, neighbours: Vector[String], router: Router, message: Message, checked: Map[String, String], through: Map[String, String]): Message = {
                var mess = retrieveClose(node, neighbours, message)
                if (mess.messageType == RETRIEVE_SUCCESS)
                    mess
                else {
                    //val neighMap = ((neigh.map(n => (n, "checked"))).toList ++ checked.toList).toMap
                    for (n <- neighbours) breakable {
                        if (!(through.contains(n))) {
                            val neighFromN = router.sendMessage(node, n, new Message(node, GET_NEIGHBOURS)).data.split(" ").toVector
                            mess = retrieval2(n, neighFromN, router, message, checked, through + (node -> "through"))
                            if (mess.messageType == RETRIEVE_SUCCESS) break
                            }
                        else mess = new Message(node, RETRIEVE_FAILURE)
                        }
                    mess
                    }
                }*/
                def addtoMap(elems: Array[String], visited: Map[String, String]): Map[String, String] = elems.isEmpty match {
                    case true => visited
                    case _ => addtoMap(elems.tail, visited + (elems.head -> "visited"))

                }

                def retrievalBfs(node: String, message: Message, queue: Queue[String], visited: Map[String, String], res: Message): Message = queue.isEmpty match {
                    case true => res
                    case _ => {
                        val n = queue.dequeue
                        val tmp = router.sendMessage(node, n, message)
                        if (tmp.messageType == RETRIEVE_SUCCESS) tmp
                        else {
                            val neighFromN = router.sendMessage(node, n, new Message(node, GET_NEIGHBOURS)).data.split(" ").filter(n => !(visited.contains(n)))
                            var q = queue
                            for (n <- neighFromN){ 
                            q.enqueue(n) }
                            retrievalBfs(n, message, q, addtoMap(neighFromN, visited), tmp)
                        }
                    }
                }

            response // Return the correct response message
        }
        else if (message.messageType == STORE) { // Request to store key->value

            /*
             * TODO: task 2.2
             * Change the storage algorithm below to store on the peers
             * when there isn't enough space on the HOST node.
             *
             * TODO: task 2.3
             * Change the storage algorithm below to handle nodes crashing.
             */
            def addtoMap(elems: Array[String], visited: Map[String, String]): Map[String, String] = elems.isEmpty match {
                    case true => visited
                    case _ => addtoMap(elems.tail, visited + (elems.head -> "visited"))

                }

            def findNodetoStoreBfs(node: String, message: Message, queue: Queue[String], visited: Map[String, String], res: Vector[String]): Vector[String] = queue.isEmpty match {
                case true => res
                case _ => {
                    val n = queue.dequeue
                    var tmp = res
                    if (router.sendMessage(node, n, new Message(node, GET_BOOLMEMORY)).data.toBoolean) {
                        tmp = tmp :+ n
                        if (res.length == 6) tmp
                    }
                    val neighFromN = router.sendMessage(node, n, new Message(node, GET_NEIGHBOURS)).data.split(" ").filter(n => !(visited.contains(n)))
                    var q = queue
                    for (n <- neighFromN){ 
                    q.enqueue(n) }
                    findNodetoStoreBfs(n, message, q, addtoMap(neighFromN, visited), tmp)
                }
            }
            
            /*def findNodetoStoreClose(node: String, neighbours: Vector[String], checked: Vector[String], through: Vector[String], res: Vector[String]): Vector[String] =(neighbours.isEmpty || res.length == 6) match{
                case true => res
                case _ => {
                    val n = neighbours.head
                    if (router.sendMessage(node, n, new Message(node, GET_BOOLMEMORY)).data.toBoolean)
                        findNodetoStore(node, neighbours.tail, checked, through, res :+ n)
                    else findNodetoStore(node, neighbours.tail, checked, through, res)
                } 
            }

            def findNodetoStore(node: String, neighbours: Vector[String], checked: Vector[String], through: Vector[String], res: Vector[String]): Vector[String] = res.length == 6 match {
                case true => res
                case _ => 
                    var tmp = findNodetoStoreClose(node, neighbours, checked, through, res)
                    if (tmp.length == 6)
                        tmp
                    else
                        breakable{
                            for (n <- neighbours) {
                                if (!(through.contains(n))) {
                                val neighFromN = router.sendMessage(node, n, new Message(node, GET_NEIGHBOURS)).data.split(" ").toVector
                                tmp = findNodetoStore(n, neighFromN, checked, through :+ node, tmp)
                                if (tmp.length == 6) break

                                }                               
                            }
                        }
                        tmp              
                    }*/

            /*def storeInNeighbours(node: String, neighbours: Vector[String], router: Router, message: Message, checked: Vector[String], k: Int): Int = (neighbours.isEmpty || k ==0) match {
                case true => k
                case _ => {
                val n = neighbours.head
                if (!(checked.contains(n)) && router.sendMessage(node, n, message).messageType == STORE_SUCCESS)
                    storeInNeighbours(node, neighbours.tail, router, message, checked, k - 1)
                else storeInNeighbours(node, neighbours.tail, router, message, checked, k)
                }
            }

            def goToNeighbours(node: String, neighbours: Vector[String], router: Router, message: Message, checked: Vector[String], through: Vector[String], k: Int): Int = (neighbours.isEmpty || k ==0) match {
                case true => k
                case _ => {
                    val n = neighbours.head
                    if (!(through.contains(n))) {
                        val neighFromN = router.sendMessage(node, n, new Message(node, GET_NEIGHBOURS)).data.split(" ").toVector
                        val left = storeInNeighbours(n, neighFromN, router, message, checked, k)
                        if (left == 0)
                            0
                        else
                            goToNeighbours(node, neighbours.tail, router, message, checked.union(neighFromN).distinct, through :+ n, left)             
                    }
                    else goToNeighbours(node, neighbours.tail, router, message, checked, through, k)
                }
            }

            def storage(node: String, neighbours: Vector[String], router: Router, message: Message, checked: Vector[String], through: Vector[String], k: Int): Message = {
                var m = new Message(id, STORE_FAILURE)
                val left = storeInNeighbours(node, neighbours, router, message, checked, k)
                if (left == 0)
                    m = new Message(id, STORE_SUCCESS)
                else {
                    neighbours.foreach(neighbour =>{
                        val neighFromN = router.sendMessage(node, neighbour, new Message(node, GET_NEIGHBOURS)).data.split(" ").toVector

                    })
                }     
                /*println(m.messageType)
                if ((m.messageType == STORE_SUCCESS))*/
                m
                        /*else {
                            val anySucess = neighbours.map(n =>{
                                if (through.contains(n)) new Message(id, STORE_FAILURE)
                                else {
                                    val neighFromN = router.sendMessage(node, n, new Message(node, GET_NEIGHBOURS)).data.split(" ").toVector
                                    println(neighFromN)
                                    storage(n, neighFromN, router, message, (checked).union(neighbours).distinct, through :+ node)
                                }
                            }).filter(x => x.messageType == STORE_SUCCESS)
                            if (!(anySucess.isEmpty))
                                anySucess.head
                            else new Message(node, STORE_FAILURE)
                        }
                        */
                    }*/


            val data = message.data.split("->") // data(0) is key, data(1) is value
            val option = getKey(data(0))
            val storedOnSelf = setKey(data(0), data(1)) // Store on current node
            if (storedOnSelf) {
                if (from == USER) {
                    val vect = findNodetoStoreBfs(id, message, Queue[String](id) , Map[String, String]((id, "visited")), Vector[String](id)).drop(1).take(5)
                    val storing = vect.map(elem => router.sendMessage(id, elem, message))
                    if (vect.length < 5) new Message(id, STORE_FAILURE) else new Message(id, STORE_SUCCESS)
                    }
                else
                    new Message(id, STORE_SUCCESS)
                }
                else 
                    if (from == USER) {
                        val vect = findNodetoStoreBfs(id, message, Queue[String](id) , Map[String, String]((id, "visited")), Vector[String]()).take(6)
                        val storing = vect.map(elem => router.sendMessage(id, elem, message))
                        if (vect.length < 5) new Message(id, STORE_FAILURE) else new Message(id, STORE_SUCCESS)
                    } 
                    else
                        new Message(id, STORE_FAILURE)
        }
        else if (message.messageType == GET_BOOLMEMORY)
            new Message(id, BOOLMEMORY, hasMoreSpace().toString)
        /*
         * Feel free to add more kinds of messages.
         */
        else
            new Message(id, INTERNAL_ERROR)
    }
}