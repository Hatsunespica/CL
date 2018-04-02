#include <iostream>
#include <cstring>
#include <algorithm>

using namespace std;

struct Node{
    int v;
    Node* le,*ri;
    explicit Node(int value=0,Node* lef=nullptr,Node* righ=nullptr):
        v(value),le(lef),ri(righ){}
}*root;

int n,m,x,y;
char c;

Node* init(int le,int ri){
    if(le==ri)
        return new Node(GIVEN_VALUE);
    else{
        int m=(le+ri)>>1;
        Node* lef=init(le,m),*rig = init(m+1,ri);
        return new Node(max(lef->v,rig->v),lef,rig);
    }
}

Node* update(Node* st,int le,int ri){
    if(le==ri)
        return new Node(y);
    else{
        int m=(le+ri)>>1;
        Node* tmp;
        if(x<=m){
            tmp=update(st->le,le,m);
            return new Node(max(tmp->v,st->ri->v),tmp,st->ri);
        }else{
            tmp=update(st->ri,m+1,ri);
            return new Node(max(tmp->v,st->le->v),st->le,tmp);
        }
    }
}

int query(Node* st,int le,int ri){
    int m=(le+ri)>>1;
    if(x<=le&&ri<=y)
        return st->v;
    if(y<=m)
        return query(st->le,le,m);
    if(x>m)
        return query(st->ri,m+1,ri);
    if(le<x||ri>y)
        return max(query(st->le,le,m),query(st->ri,m+1,ri));
}

int main(){
    cin>>n>>m;
    root=init(1,n);
    while(m--){
        cin>>c>>x>>y;
        if(c=='Q')
            cout<<query(root,1,n)<<endl;
        else
            root=update(root,1,n);
    }
}
