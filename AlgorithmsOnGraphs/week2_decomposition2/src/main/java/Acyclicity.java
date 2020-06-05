import java.util.ArrayList;
import java.util.Scanner;

public class Acyclicity {
    private static int acyclic(ArrayList<Integer>[] adj) {
        boolean[] visited = new boolean[adj.length];
        boolean[] recStack = new boolean[adj.length];
        boolean result = false;

        return 0;
    }

    private static boolean isCyclic(boolean[] visited,
                                    boolean[] recStack,
                                    ArrayList<Integer>[] adj,
                                    int u) {
        // if already marked then we have cycle
        if (recStack[u]) {
            return true;
        }
        if (visited[u]) {
            return false;
        }

        // Mark the current node as visited and part of recursion stack
        recStack[u] = true;
        visited[u] = true;

        for (Integer v : adj[u]) {
            if (isCyclic(visited, recStack, adj, v)) {
                return true;
            }
        }
        recStack[u] = false;
        return false;
    }

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        int n = scanner.nextInt();
        int m = scanner.nextInt();
        ArrayList<Integer>[] adj = (ArrayList<Integer>[]) new ArrayList[n];
        for (int i = 0; i < n; i++) {
            adj[i] = new ArrayList<Integer>();
        }
        for (int i = 0; i < m; i++) {
            int x, y;
            x = scanner.nextInt();
            y = scanner.nextInt();
            adj[x - 1].add(y - 1);
        }
        System.out.println(acyclic(adj));
    }
}

