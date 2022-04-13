// Robin Jean-Sébastien Yvon Debalme
package task2

import scala.io.Source

object Task2d {
  /*
   * Main Class. This will be used to test your code.
   * These are just example queries and tests. The final tests are private.
   */

   /*
    * Do not modify the following messageType strings.
    * You can however add any more you like.
    */
    val STORE = "STORE"
    val STORE_SUCCESS = "STORE_SUCCESS"
    val STORE_FAILURE = "STORE_FAILURE" // You should never have to return this.
    val RETRIEVE = "RETRIEVE"
    val GET_NEIGHBOURS = "GET_NEIGHBOURS"
    val NEIGHBOURS_RESPONSE = "NEIGHBOURS_RESPONSE"
    val RETRIEVE_SUCCESS = "RETRIEVE_SUCCESS"
    val RETRIEVE_FAILURE = "RETRIEVE_FAILURE"
    val INTERNAL_ERROR = "INTERNAL_ERROR"
    val USER = "USER"
    val nodes_may_fail = 4

  def main(args: Array[String]) {
    val router: Router = new Router() // Instantiate the router

    // Read the overlay adjacency list
    val source = Source.fromFile("overlay5.txt")
    val routerInfo: scala.collection.mutable.Map[String, MyNode] = scala.collection.mutable.Map()
    for (line <- source.getLines()) {
      val adj = line.split(" ")
      val myNode = new MyNode(adj(0), adj(1).toInt, adj.drop(2).toVector, router)
      routerInfo += (adj(0) -> myNode)
    }
    source.close()

    router.addNodes(routerInfo.toMap) // Add new nodes to the router

    /* QUERIES */

    router.sendMessage(USER, "uD", new Message(USER, STORE, "key6->value6")) // STORE_SUCCESS
    router.sendMessage(USER, "u3", new Message(USER, STORE, "key7->value7")) // STORE_SUCCESS
    router.sendMessage(USER, "uD", new Message(USER, STORE, "key8->value8")) // STORE_SUCCESS
    router.sendMessage(USER, "u3", new Message(USER, STORE, "key9->value9")) // STORE_SUCCESS


    /* Check the correctness of the store. The stored keys should be present in at least one node */
    println(routerInfo("u1").returnStore)
    println(routerInfo("u2").returnStore)
    println(routerInfo("u3").returnStore)
    println(routerInfo("u4").returnStore)
    println(routerInfo("u5").returnStore)
    println(routerInfo("u0").returnStore)
    println(routerInfo("uA").returnStore)
    println(routerInfo("uB").returnStore)
    println(routerInfo("uC").returnStore)
    println(routerInfo("uD").returnStore)
    println(routerInfo("uE").returnStore)
    println(routerInfo("uF").returnStore)


    /* Crash some nodes*/
    var failing_node = routerInfo("u1")
    var all_keys = failing_node.returnStore.keySet
    all_keys.foreach {
      failing_node.removeKey
    }

    println("Post Crash")

    println(routerInfo("u1").returnStore)
    println(routerInfo("u2").returnStore)
    println(routerInfo("u3").returnStore)
    println(routerInfo("u4").returnStore)
    println(routerInfo("u5").returnStore)
    println(routerInfo("u0").returnStore)
    println(routerInfo("uA").returnStore)
    println(routerInfo("uB").returnStore)
    println(routerInfo("uC").returnStore)
    println(routerInfo("uD").returnStore)
    println(routerInfo("uE").returnStore)
    println(routerInfo("uF").returnStore)



  }
}
