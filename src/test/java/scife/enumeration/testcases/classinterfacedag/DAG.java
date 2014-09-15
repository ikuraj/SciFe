package scife.enumeration.testcases.classinterfacedag;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

public class DAG {

    /**
     * Nodes list contains all nodes of a DAG
     */
    //transient fields are skipped during visualization 
    transient public List<DAGNode> nodes = new LinkedList<DAGNode>();
    
    public List<DAGNode> getNodes() {
        return nodes;
    }
    
    public void setNodes(List<DAGNode> nodes) {
        this.nodes = nodes;
    }

    /**
     * Number of nodes in a DAG. 
     */
    int size;

    public int getSize() {
        return size;
    }
    
    public void setSize(int size) {
        this.size = size;
    }

    int methodsNum;


  /**
   * Checks for basic DAG properties. Checks if graph contains loops. If it
   * does, returns false, otherwise returns true.
   */
  public boolean checkIfDag() {
      Set<DAGNode> visited = new HashSet<DAGNode>();
      Set<DAGNode> path = new HashSet<DAGNode>();
      
//      System.out.println("numOfExtendsPerNode" + numOfExtendsPerNode);

      for (int i=0; i< getNodes().size(); i++) {
        DAGNode node = getNodes().get(i);

        if (node == null) return false;
        if (node.allBools == null) return false;
        boolean d = node.isClass;
        for (int j=0; j< size + 2*methodsNum; j++) {
        	boolean dummy = node.allBools[j];
        }
        int d2 = node.numChildren;
      }

      for (int i=0; i< getNodes().size(); i++) {
        DAGNode node = getNodes().get(i);

          if (node == null) return false;
          if (node.allBools == null) return false;
//          if (node.getOverriden() == null) return false;

        if (!visited.contains(node)) {
            if (!checkNextNode(path, visited, i)) {
                return false;
            }
        }
      }
//      if (!visited.contains(getExtendedClass())) {
//        if (!checkNextNode(path, visited, getExtendedClass())) {
//            return false;
//        }
//      }
      boolean ok = size == visited.size();
      return ok;
  }

  public boolean checkNextNode(Set<DAGNode> path, Set<DAGNode> visited, int index) {
    DAGNode n = getNodes().get(index);
    if (path.contains(n)) return false;
//    System.out.println("visiting: " + n);

  // sealed methods should not be overriden
  if (!checkNoOverrideOfSeal(path, visited, n)) return false;
  // cannot seal without redefining
  if (!checkCannotSealWithoutAnOverride(path, visited, n)) return false;
//
//  // check interface to class extension
//  if (!n.isClass) {
//    for (int i = 0; i < n.getNumberOfChildren(); i++) {
//      DAGNode child = getNodes().get(i);
//      
//      if (child.isClass) return false;
//    }
//  }
    boolean dummy = n.isClass;
    
    path.add(n);
    for (int i = 0; i < n.getNumberOfChildren(); i++) {
        boolean isChild = n.isChild(i);
        DAGNode child = getNodes().get(i);

        // you cannot point to a lower index
        if (isChild && i <= index)
              return false;

        // check if path contains a class
//        if (n == child) {
//          System.out.println("i=" + i + " index=" + index);
//          throw new java.lang.RuntimeException();
//        }
        if (isChild && !checkExtends(path, visited, n, child)) return false;

        // check property for every child of this node
        // this condition is not needed since cycles cannot occur (see path.contains at method beginning)
        // visited part of condition hurts correctness actually - you *need* visit each parent before you can
        // claim correctness of the child
//        if (!visited.contains(child) && !path.contains(child))
          if (!visited.contains(child) && isChild && !checkNextNode(path, visited, i))
              return false;
    }

    // visited after the cycle computation is done
    visited.add(n);

    path.remove(n);
    return true;
    }


    public boolean checkExtends(Set<DAGNode> path, Set<DAGNode> visited, DAGNode n, DAGNode child) {
      // this node is a class, check children for correctness
      if (n.isClass) {
        int numExtendsForThisChild = numOfExtendsPerNode.get(child) + 1;

        if (!child.isClass && numExtendsForThisChild > 0) {
//          System.out.println("bad: " + (!child.isClass && numExtendsForThisChild > 0));
//          System.out.println("numExtendsForThisChild" + numExtendsForThisChild);
//          System.out.println(this.toString());
          return false;
        }
        if (child.isClass && numExtendsForThisChild > 1) {
//          System.out.println("bad2: " + (!child.isClass && numExtendsForThisChild > 0));
//          System.out.println("numExtendsForThisChild" + numExtendsForThisChild);
//          System.out.println(this.toString());
          return false;
        }

        numOfExtendsPerNode.put(child, numExtendsForThisChild);
      } else {
//        if (child.isClass) return false;
      }
      return true;
    }

    public boolean checkCannotSealWithoutAnOverride(Set<DAGNode> path, Set<DAGNode> visited, DAGNode n) {
      for (int i=0; i<methodsNum; i++) {
      	boolean flag = !n.getOverriden(i); // force me otherwise I will not be enumerated...
        if (n.getSealed(i) && flag) return false;
      }
      return true;
    }

    public boolean checkNoOverrideOfSeal(Set<DAGNode> path, Set<DAGNode> visited, DAGNode n) {
      boolean[] finalized = new boolean[methodsNum];
      // not sure if java will initialize to false, so we will do it
      for (int i=0; i<methodsNum; i++) finalized[i] = false;

      for (DAGNode pathNode: path) {
        for (int i=0; i<methodsNum; i++) {
          if (pathNode.getSealed(i)) finalized[i] = true;
        }
      }

      for (int i=0; i<methodsNum; i++) {
        if (finalized[i] && n.getOverriden(i)) return false;
      }

      return true;
    }

    private HashMap<DAGNode, Integer> numOfExtendsPerNode = new HashMap<DAGNode, Integer>();

    public boolean repOK() {
//      System.out.println("new run -----------------");
      for (DAGNode n: nodes) {
        numOfExtendsPerNode.put(n, 0);
      }
      return checkIfDag();
    }

    public DAGNode getExtendedClass() {
      return this.nodes.get(0);
    }

    public void setExtendedClass(DAGNode extendedClass) {
      this.nodes.set(0, extendedClass);
    }

    public int getMethodsNum() {
      return methodsNum;
    }

    public void setMethodsNum(int methodsNum) {
      this.methodsNum = methodsNum;
    }

    @Override
    public String toString() {
        String ans = "";
        for (DAGNode n : nodes) {
//            ans += n.toString() + "\n";
            ans += n.toString() + " ";
        }
//        ans += "------------------------";
        return ans;
    }

}
