int main(){
int n = 1;
int c1 = 0;
int c2 = 0;
int c3 = 0;
int c4 = 0;
while(n > 0){
if(n >= 0 <= n <= 25) goto l0;
l0:{
c1 = c1 + 1;
}
if(n >= 26 <= n <= 50) goto l1;
l1:{
c2 = c2 + 1;
}
if(n >= 51 <= n <= 75) goto l2;
l2:{
c3 = c3 + 1;
}
if(n >= 76 <= n <= 100) goto l3;
l3:{
c4 = c4 + 1;
}
}
return 0;
}
