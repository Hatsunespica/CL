#include <iostream>
#include <cstring>
#include <cstdlib>
#include <ctime>

using namespace std;

struct Node{
    int v;
    Node* le,*ri;
    explicit Node(int value=0,Node* lef=nullptr,Node* righ=nullptr)
            :v(value),le(lef),ri(righ){};
}*root;

Node* bst_insert(int value, Node* bst),* bst_find(int value, Node* bst);
void dfs(Node* bst);

int main(){
    srand(time(0));
    for(int i=0;i<10;++i)
        root=bst_insert(rand()%100,root);
    dfs(root);
    return 0;
}

Node* bst_insert(int value,Node* bst){
    if(bst==nullptr)
        return new Node(value);
    if(value==bst->v)
        return bst;
    else if(value<bst->v)
        return new Node(bst->v,bst_insert(value,bst->le),bst->ri);
    else
        return new Node(bst->v,bst->le,bst_insert(value,bst->ri));
}

Node* bst_find(int value,Node* bst){
    if(bst==nullptr)
        return nullptr;
    if(value==bst->v)
        return bst;
    else
        return (value<bst->v)?bst_find(value,bst->le):bst_find(value,bst->ri);
}

void dfs(Node* bst){
    if(bst!=nullptr){
        cout<<bst->v<<' ';
        dfs(bst->le);dfs(bst->ri);
    }
}