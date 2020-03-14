import java.util.LinkedList;
import java.util.Queue;
import java.util.Scanner;

class Node{
    public int x;
    public int y;
    public int step;
    public Node(int x,int y,int step){
        this.x=x;
        this.y=y;
        this.step=step;
    }
}

public class Main {
    private static int[][] stepArr = new int[][] { {-1,0},{1,0},{0,-1},{0,1} };

    private static int bfs(int[][] map, int n) {
        Node node = new Node(1, 1, 0);
        Queue<Node> q = new LinkedList<>();
        q.add(node);
        while (q.size() != 0)
        {
            node = q.remove();
            if (node.x == n - 2 && node.y == n - 2)
            {
                return node.step;
            }
            map[node.x][node.y] = 2;
            for (int i = 0; i < 4; i++)
            {
                int x = node.x + stepArr[i][0];
                int y = node.y + stepArr[i][1];
                if (x >= 0 && y >= 0 && x < n&&y < n&& map[x][y] == 0)
                {
                    map[x][y] = 2;
                    Node next = new Node(x, y, node.step + 1);
                    q.add(next);
                }
            }
        }
        return -1;
    }
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        int n = scanner.nextInt();
        int[][] map = new int[n][n];
        for (int i = 0;i<n;++i){
            for (int j = 0;j<n;++j){
                map[i][j]=scanner.nextInt();
            }
        }
        int r = bfs(map,n);
        System.out.println(r==-1?"No solution":r);
    }
}
