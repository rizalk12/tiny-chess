import json,os,urllib.request as U
V={'P':100,'N':320,'B':330,'R':500,'Q':900,'K':0}
ND=((-2,-1),(-2,1),(-1,-2),(-1,2),(1,-2),(1,2),(2,-1),(2,1))
BD=((-1,-1),(-1,1),(1,-1),(1,1));RD=((-1,0),(1,0),(0,-1),(0,1))
KD=BD+RD;DT={'N':ND,'B':BD,'R':RD,'Q':KD,'K':KD}
MATE=100000;INF=10**9;LIM=[20000];N=[0];TT={}
CMAP={'e1g1':4028,'e1c1':3772,'e8g8':388,'e8c8':132}
AB=Exception
def pst(u,s):
 f=s&7;rr=7-(s>>3);c=min(f,7-f)+min(rr,7-rr)
 if u=='P':return (0,0,5,10,20,35,70,0)[rr]
 if u=='N':return -15+7*c
 if u in 'BQ':return -5+(4 if u=='B' else 2)*c
 if u=='R':return 25*(rr==6)
 return 12-7*c
def C(p,i):
 v=V[p.upper()]+pst(p.upper(),i if p.isupper() else i^56)
 return v if p.isupper() else -v
def G(bd,w,caps):
 ms=[];ap=ms.append
 for i,p in enumerate(bd):
  if p=='.' or p.isupper()!=w:continue
  u=p.upper();f=i&7;r=i>>3
  if u=='P':
   d=-8 if w else 8;pr=1 if w else 6;one=i+d
   if bd[one]=='.':
    if r==pr:ms+=[i|one<<6|x<<12 for x in (4,3,2,1)]
    elif not caps:
     ap(i|one<<6)
     if r==(6 if w else 1) and bd[i+2*d]=='.':ap(i|(i+2*d)<<6)
   for df in (-1,1):
    if not 0<=f+df<8:continue
    t=one+df;q=bd[t]
    if q!='.' and q.isupper()!=w:
     if r==pr:ms+=[i|t<<6|x<<12 for x in (4,3,2,1)]
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
    if u in 'NK':break
    nf+=df;nr+=dr
 return ms
def A(bd,s,w):return any((m>>6)&63==s for m in G(bd,w,True))
def MK(pos,m):
 bd,w,sc=pos;a=m&63;b=(m>>6)&63;pr=m>>12;p=bd[a];q=bd[b]
 d=C(p,b)-C(p,a)
 if q!='.':d-=C(q,b)
 L=list(bd);L[a]='.';L[b]=p
 if pr:
  x='NBRQ'[pr-1] if w else 'nbrq'[pr-1];d+=C(x,b)-C(p,b);L[b]=x
 if p in 'Kk' and abs(b-a)==2:
  rf,rt=(63,61) if b==62 else (56,59) if b==58 else (7,5) if b==6 else (0,3)
  d+=C(bd[rf],rt)-C(bd[rf],rf);L[rt]=bd[rf];L[rf]='.'
 return (''.join(L),not w,sc+d)
def O(bd,ms,ttm):
 def key(m):
  if m==ttm:return 1<<30
  b=(m>>6)&63;q=bd[b];pr=m>>12
  if q!='.' or pr:return (V[q.upper()] if q!='.' else 0)*16-V[bd[m&63].upper()]+(900 if pr else 0)
  return 0
 ms.sort(key=key,reverse=True)
def S(pos,depth,a,bt,ply):
 bd,w,sc=pos;k=bd.find('K' if w else 'k')
 if k<0:return -MATE+ply
 chk=A(bd,k,not w)
 N[0]+=1
 if N[0]>LIM[0]:raise AB()
 ev=sc if w else -sc;ttm=0
 if depth<=0 and (not chk or depth<0):
  if ev>=bt:return ev
  if ev>a:a=ev
  best=ev;ms=G(bd,w,True)
 else:
  if depth>0:
   e=TT.get((bd,w));ttm=e[3] if e else 0
   if e and e[0]>=depth:
    d0,v0,f0=e[0],e[1],e[2]
    if f0==0 or (f0==1 and v0>=bt) or (f0==2 and v0<=a):return v0
  best=-INF;ms=G(bd,w,False)
  if not ms:return -MATE+ply if chk else 0
 O(bd,ms,ttm);a0=a;bm=ms[0] if ms else 0
 for m in ms:
  v=MATE-ply if bd[(m>>6)&63] in 'Kk' else -S(MK(pos,m),depth-1,-bt,-a,ply+1)
  if v>best:best=v;bm=m
  if v>a:a=v
  if a>=bt:break
 if depth>0:TT[(bd,w)]=(depth,best,1 if best>=bt else 2 if best<=a0 else 0,bm)
 return best
def parse(fen):
 P=fen.split()
 bd=''.join('.'*int(c) if c.isdigit() else c for row in P[0].split('/') for c in row)
 sc=sum(C(p,i) for i,p in enumerate(bd) if p!='.')
 return (bd,P[1]=='w',sc)
def uci(m):
 a=m&63;b=(m>>6)&63;pr=m>>12
 return chr(97+(a&7))+str(8-(a>>3))+chr(97+(b&7))+str(8-(b>>3))+('nbrq'[pr-1] if pr else '')
def choose(fen,lu):
 pos=parse(fen);by={uci(m):m for m in G(pos[0],pos[1],False)}
 cand=[by.get(u,CMAP.get(u)) for u in lu if u in by or u in CMAP];best=cand[0];TT.clear();N[0]=0
 for depth in range(1,41):
  a=-INF;N[0]=0
  try:
   O(pos[0],cand,best)
   for m in cand:
    v=-S(MK(pos,m),depth-1,-INF,INF,1)
    if v>a:a=v;best=m
  except AB:break
  cand.remove(best);cand.insert(0,best)
 return uci(best)
def main():
 E=os.environ["MAKEFUN_ENV_URL"]
 def post(p2,body):
  return json.load(U.urlopen(U.Request(E+p2,data=json.dumps(body).encode(),headers={"content-type":"application/json"})))
 while True:
  g=post("/game",{})
  if g["status"]=="done":break
  while g["status"]=="your_move":g=post("/move",{"game":g["game"],"move":choose(g["fen"],g["legal_moves"])})
main()
