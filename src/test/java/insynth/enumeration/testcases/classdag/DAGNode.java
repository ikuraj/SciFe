package insynth.enumeration.testcases.classdag;



/**
 * Node for Direct Acyclic Graph. Contains array of DAGNodes, that represent the
 * children of a given DAGNode.
 *
 * @see DAG
 *
 */
public class DAGNode {

    private static int autoId = 0;

    transient int id = autoId++;

    // always a class
//    boolean isClass;
    
    public int numChildren;
    // children then overriden then sealed
    public boolean[] allBools;
//    private boolean[] overriden;
//    private boolean[] sealed;

    // because cannot pass finitized array fields to other methods
    @Override
    public String toString() {
//        String ans = "DAGNode [children=";
        String ans = "n[";
        ans += "e=";

            String ans1 = "[";
            for (int i = 0; i < getNumberOfChildren(); i++) {
                if (i > 0)
                    ans1 += ", ";
                ans1 += isChild(i);
            }
            ans1 += "]";
            ans += ans1;
//        ans += ", isClass=" + isClass +
        ans += ", o";
        String ans2 = "=";
        for (int i = 0; i < getNumberOfOverriden(); i++) {
            if (i > 0) ans2 += ", ";
            ans2 += getOverriden(i);
        }
        ans += ans2;
        ans += ", s";
        String ans3 = "=";
        for (int i = 0; i < getNumberOfSealed(); i++) {
            if (i > 0) ans3 += ", ";
            ans3 += getSealed(i);
        }
        ans += ans3;
        ans += "]";
        return ans;
    }


		public boolean getOverriden(int i) {
			return allBools[numChildren + i];
		}

		public boolean getSealed(int i) {
			return allBools[numChildren + getNumberOfOverriden() + i];
		}

    public boolean isChild(int i) {
      return allBools[i];
    }

    public int getNumberOfChildren() {
      return numChildren;
    }

    public int getNumberOfSealed() {
      return (allBools.length - numChildren)/2;
    }

    public int getNumberOfOverriden() {
      return (allBools.length - numChildren)/2;
    }



}
