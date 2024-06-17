#include <bits/stdc++.h>
using namespace std;

#define pi pair<int, int>
#define inf32 0x3f3f3f3f
#define inf64 0x3f3f3f3f3f3f3f3f
#define all(x) x.begin(), x.end()
#define Unique(v) v.erase(unique(all(v)), v.end())
#define setinf(d) memset(d, inf32, sizeof d)
#define setneg(d) memset(d, -1, sizeof d)
#define set0(d) memset(d, 0, sizeof d)
#define Log2(x) 63 - __builtin_clzll(x)
#define oo 2e18
#define mod 1000000007
#define FILENAME "f"

const int MAXN = 1173;

double dis[MAXN][MAXN];

namespace std {
    template <> struct hash<std::pair<int, int>> {
        inline size_t operator()(const std::pair<int, int> &v) const {
            std::hash<int> int_hasher;
            return int_hasher(v.first) ^ int_hasher(v.second);
        }
    };   
}


struct OneMinimumSpanningTree {
    int sourceNode;
    int numNodes;
    vector<int> par;
    vector<double> d;
    vector<int> topo;
    vector<bool> visited;
    unordered_set<pi> edges;
    vector<vector<int>> g;
    int chooseRate;

    vector<int> bestPar;
    vector<int> bestTopo;
    vector<double> bestW;
    unordered_set<pi> bestEdges;

    /*
     * Implement Prim Algorithm to find the minimum spanning tree without the source node
     * Find the two nodes that have the minimum distance to source node -> A 1-Tree Minimum Spanning Tree is found.
     * For all edge (i, j), find beta(i, j) and compute alpha(i, j) = beta(i, j) - dis(i, j)
     * The function will return for each node the 0.1 * numNodes closest to it base on alpha
    */

    OneMinimumSpanningTree(int _sourceNode, int _numNodes, int _chooseRate): 
        sourceNode(_sourceNode), 
        numNodes(_numNodes),
        d(_numNodes),
        par(_numNodes),
        visited(_numNodes),
        chooseRate(_chooseRate),
        g(_numNodes) {}

    void reset(){
        for (int i = 0; i < numNodes; i++) {
            d[i] = inf32;
            visited[i] = false;
            par[i] = -1;
            g[i].clear();
        }
        topo.clear();
        edges.clear();
    }

    double primAlgorithm(vector<double> &w) {
        // Remember to exclude the source node
        reset();
        double totalWeight = 0;

        d[sourceNode + 1] = 0;
        
        for (int iter = 1; iter <= numNodes - 1; iter++) {
            int bestNode;
            double Min = inf32 + 1;

            for (int node = 1; node < numNodes; node++){
                if (!visited[node] && d[node] < Min){
                    Min = d[node];
                    bestNode = node;
                }
            }

            totalWeight += d[bestNode];
            topo.push_back(bestNode);
            if (par[bestNode] != -1){
                edges.insert({bestNode, par[bestNode]});
                g[par[bestNode]].push_back(bestNode);
                g[bestNode].push_back(par[bestNode]);
            }
            visited[bestNode] = true;

            for (int node = 1; node < numNodes; node++){
                if (!visited[node] && d[node] > dis[bestNode][node] + w[bestNode] + w[node]) {
                    d[node] = dis[bestNode][node] + w[bestNode] + w[node];
                    par[node] = bestNode;
                }
            }
        }
        checkValid();
        return totalWeight;
    }
    

    void dfs(int node, int p, vector<bool> &vis){
        vis[node] = true;
        for (auto &v : g[node]){
            if (p == v) continue;
            if (!vis[v]){
                if (par[v] != node){

                }
                dfs(v, node, vis);
            } else {
                assert(false);
            }
        }
    }

    void checkValid(){
        vector<bool> vis(numNodes);
        dfs(1, 0, vis);
        for (int i = 1; i < numNodes; i++){
            if (!vis[i]){
                assert(false);
            }
        }
    }

    void a_improved(){
        vector<double> w(numNodes, 0);
        double W = -1e18;
        double step_size = 1;
        int period = numNodes / 2;
        int iter = period;
        bool isFirstPeriodIncreasing = true;
        vector<int> prev_v(numNodes, 0);
        

        const double eps = 1e-6;
        while (period > 0 && step_size > eps){
            iter--;
            double newWeight = primAlgorithm(w);
            newWeight -= 2 * accumulate(all(w), 0.0);
            bool increase = false;

            if (newWeight > W){
                bestPar = par;
                bestTopo = topo;
                bestW = w;
                W = newWeight;
                bestEdges = edges;
                increase = true;
            }
            isFirstPeriodIncreasing &= increase;


            // compute for the next
            vector<int> v(numNodes);

            bool checkFullZero = true;
            for (int i = 0; i < numNodes; i++){
                v[i] = (int)g[i].size() - 2;
                if (v[i] != 0) checkFullZero = false;
            }

            if (checkFullZero) break;

            if (isFirstPeriodIncreasing){
                step_size *= 2;
            }

            for (int i = 0; i < numNodes; i++){
                w[i] += step_size * v[i];
            }

            if (iter == 0){
                if (increase){
                    iter = period;
                } else{
                    period /= 2;
                    step_size /= 2;
                    iter = period;
                }
            }
        }
    }

    double new_dis(int i, int j){
        if (i == j) return 0;
        return dis[i][j] + bestW[i] + bestW[j];
    }
    vector<vector<int>> computeCandidateSet() {
        a_improved();
        vector<vector<int>> candidateSet;

        // find the best two
        vector<pair<double, int>> c;
        for (int node = 1; node < numNodes; node++) {
            c.push_back({new_dis(sourceNode, node), node});
        }
        sort(all(c));

        bestEdges.insert({sourceNode, c[0].second});
        bestEdges.insert({sourceNode, c[1].second});

        vector<pair<double, int>> candidates;

        for (int i = 1; i < numNodes; i++) {
            candidates.push_back({new_dis(i, sourceNode) - new_dis(sourceNode, c[1].second), i});
        }

        sort(all(candidates));
        vector<int> candidate;

        for (int j = 0; j < numNodes / chooseRate - 2; j++){
            candidate.push_back(candidates[j].second);
        }
        candidateSet.push_back(candidate);

        candidates.clear();
        candidate.clear();

        vector<int> marked(numNodes);
        vector<double> b(numNodes);
        for (int i = 1; i < numNodes; i++){
            marked[i] = 0;
        }

        for (int node = 1; node < numNodes; node++) {
            ///// BUILD THE 1-TREE //////////////////////////
            b[node] = -INT_MAX;
            for (int k = node; k != 1; k = bestPar[k]) {
                b[bestPar[k]] = max(b[k], new_dis(k, bestPar[k]));
                marked[k] = node;
            }

            for (auto &v : bestTopo) {
                if (v != node) {
                    if (marked[v] != node) {
                        b[v] = max(b[bestPar[v]], new_dis(v, bestPar[v]));
                    }
                }
            }
            /////////////////////////////////////////////////
            for (int v = 1; v < numNodes; v++) {
                if (v == node) continue;
                candidates.push_back({new_dis(node, v) - b[v], v});
            }
            sort(all(candidates));

            for (int j = 0; j < numNodes / chooseRate - 2; j++){
                candidate.push_back(candidates[j].second);
            }
        
            candidateSet.push_back(candidate);
            candidate.clear();
            candidates.clear();
        }
        return candidateSet;
    }
};


double x[MAXN], y[MAXN];

double _dis(int i, int j){
    return ceil(sqrt(((x[i] - x[j]) * (x[i] - x[j])  + (y[i] - y[j]) * (y[i] - y[j]))));
}

int main() {
    freopen("./tests/tc/pr1002.tsp", "r", stdin);

    int num_node;
    cin >> num_node;


    for (int i = 0; i < num_node; i++) {
        cin >> x[i] >> x[i] >> y[i];
    }


    for (int i = 0; i < num_node; i++) {
        for (int j = 0; j < num_node; j++){
            dis[i][j] = _dis(i, j);
        }
    }

    int chooseRate = 1;

    OneMinimumSpanningTree mst(0, num_node, chooseRate);
   
    vector<vector<int>> candidateSet = mst.computeCandidateSet();
    ifstream inp("tests/opt/pr1002.opt.tour");

    vector<int> optimalSet;
    for (int i = 0; i < num_node; i++){
        int x;
        inp >> x;
        optimalSet.push_back(x - 1);
    }

    vector<int> rank(num_node);

    int average = 0;

    for (int i = 0; i < num_node - 1; i++){
        int u = optimalSet[i];
        int v = optimalSet[i + 1];

        int ranked = 0;
        for (int j = 0; j < candidateSet[u].size(); j++){
            if (candidateSet[u][j] == v){
                ranked = j + 1;
                break;
            }
        }
        average += ranked;
        rank[ranked]++;
    }
    cout << fixed << setprecision(6) << average * 1.0 / (num_node - 1) << endl;

    for (int i = 0; i < num_node; i++){
        cout << "Candidate of node " << i << " is: "; 
        for (auto &v : candidateSet[i]){
            cout << v << " ";
        }
        cout << endl;
    }
}