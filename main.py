import json,os,urllib.request as U
V=dict(zip('PNBRQK',(100,320,330,500,900,0)))
ND=(-2,-1),(-2,1),(-1,-2),(-1,2),(1,-2),(1,2),(2,-1),(2,1)
BD=(-1,-1),(-1,1),(1,-1),(1,1);RD=(-1,0),(1,0),(0,-1),(0,1)
KD=BD+RD;DT={'N':ND,'B':BD,'R':RD,'Q':KD,'K':KD}
MATE=10**5;INF=10**9;N=[0];TT={};KL={}
class X(Exception):pass
def pst(u,s):
 rr=7-(s>>3);c=min(s&7,7-(s&7))+min(rr,7-rr)
 return (0,0,5,10,20,35,80,0)[rr]if u=='P'else-15+7*c if u=='N'else-5+(4 if u=='B'else 2)*c if u in'BQ'else 25*(rr==6)if u=='R'else 12-7*c
def C(p,i):
 v=V[p.upper()]+pst(p.upper(),i if p.isupper() else i^56);return v if p.isupper() else -v
def G(bd,w,caps):
 ms=[];ap=ms.append
 for i,p in enumerate(bd):
  if p=='.' or p.isupper()!=w:continue
  u=p.upper();f=i&7;r=i>>3
  if u=='P':
   d=-8 if w else 8;pr=1 if w else 6;one=i+d
   if bd[one]=='.':
    if r==pr:ms+=[i|one<<6|x<<12 for x in(4,3,2,1)]
    elif not caps:
     ap(i|one<<6)
     if r==(6 if w else 1)and bd[i+2*d]=='.':ap(i|(i+2*d)<<6)
   for df in(-1,1):
    if not 0<=f+df<8:continue
    t=one+df;q=bd[t]
    if q!='.' and q.isupper()!=w:
     if r==pr:ms+=[i|t<<6|x<<12 for x in(4,3,2,1)]
     else:ap(i|t<<6)
   continue
  for df,dr in DT[u]:
   nf,nr=f+df,r+dr
   while 0<=nf<8 and 0<=nr<8:
    t=nr*8+nf;q=bd[t]
    if q!='.':
     if q.isupper()!=w:ap(i|t<<6)
     break
    if not caps:ap(i|t<<6)
    if u in'NK':break
    nf+=df;nr+=dr
 return ms
def MK(pos,m):
 bd,w,sc=pos;a=m&63;b=m>>6&63;pr=m>>12;p=bd[a];q=bd[b]
 d=C(p,b)-C(p,a)
 if q!='.':d-=C(q,b)
 L=list(bd);L[a]='.';L[b]=p
 if pr:
  x='NBRQ'[pr-1]if w else'nbrq'[pr-1];d+=C(x,b)-C(p,b);L[b]=x
 if p in'Kk' and abs(b-a)==2:
  rf,rt=(63,61)if b==62 else(56,59)if b==58 else(7,5)if b==6 else(0,3)
  d+=C(bd[rf],rt)-C(bd[rf],rf);L[rt]=bd[rf];L[rf]='.'
 return ''.join(L),not w,sc+d
def O(bd,ms,ttm,y):
 k1=KL.get(y,0)
 def key(m):
  if m==ttm:return 1<<30
  b=m>>6&63;q=bd[b];pr=m>>12
  if q!='.' or pr:return (V[q.upper()]if q!='.'else 100)*16-V[bd[m&63].upper()]+900*(pr>0)
  return(1<<20)*(m==k1)
 ms.sort(key=key,reverse=True)
def S(pos,d,a,bt,y):
 bd,w,sc=pos;k=bd.find('Kk'[not w])
 chk=any(m>>6&63==k for m in G(bd,not w,True));N[0]+=1
 if N[0]>26000:raise X
 ev=sc*(2*w-1);ttm=0
 if d<=0 and (not chk or d<0):
  if ev>=bt:return ev
  if ev>a:a=ev
  best=ev;ms=G(bd,w,True)
 else:
  if d>0:
   e=TT.get((bd,w));ttm=e[3]if e else 0
   if e and e[0]>=d:
    v0,f0=e[1],e[2]
    if f0==0 or f0==1 and v0>=bt or f0==2 and v0<=a:return v0
  best=-INF;ms=G(bd,w,False)
  if not ms:return -MATE+y if chk else 0
 O(bd,ms,ttm,y);a0=a
 for i,m in enumerate(ms):
  b=m>>6&63;cap=bd[b]!='.' or m>>12
  if bd[b]in'Kk':v=MATE-y
  elif i==0:v=-S(MK(pos,m),d-1,-bt,-a,y+1)
  else:
   r=d-1-(d>=3 and i>=3 and not cap and not chk)
   v=-S(MK(pos,m),r,-a-1,-a,y+1)
   if a<v<bt:v=-S(MK(pos,m),d-1,-bt,-a,y+1)
  if v>best:best=v;bm=m
  if v>a:
   a=v
   if a>=bt:
    if not cap:KL[y]=m
    break
 if d>0:TT[(bd,w)]=(d,best,1 if best>=bt else 2 if best<=a0 else 0,bm)
 return best
def parse(fen):
 P=fen.split();bd=''.join('.'*int(c)if c.isdigit()else c for row in P[0].split('/')for c in row)
 return bd,P[1]=='w',sum(C(p,i)for i,p in enumerate(bd)if p!='.')
CMAP={'e1g1':4028,'e1c1':3772,'e8g8':388,'e8c8':132}
def uci(m):
 a=m&63;b=m>>6&63;pr=m>>12
 return f"{chr(97+(a&7))}{8-a//8}{chr(97+(b&7))}{8-b//8}"+('nbrq'[pr-1]if pr else'')
def choose(fen,lu):
 pos=parse(fen);by={uci(m):m for m in G(pos[0],pos[1],False)}
 cand=[by.get(u,CMAP.get(u))for u in lu if u in by or u in CMAP];best=cand[0];TT.clear()
 for d in range(1,41):
  a=-INF;N[0]=0
  try:
   O(pos[0],cand,best,0)
   for m in cand:
    v=-S(MK(pos,m),d-1,-INF,INF,1)
    if v>a:a=v;best=m
  except X:break
  cand.remove(best);cand.insert(0,best)
 return uci(best)
def main():
 E=os.environ["MAKEFUN_ENV_URL"]
 def post(p2,body):return json.load(U.urlopen(U.Request(E+p2,data=json.dumps(body).encode(),headers={"content-type":"application/json"})))
 while True:
  g=post("/game",{})
  if g["status"]=="done":break
  while g["status"]=="your_move":g=post("/move",{"game":g["game"],"move":choose(g["fen"],g["legal_moves"])})
main()
