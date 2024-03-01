PROGRAM create_alpha_input

INTEGER :: i,j,k,pz,pa,dz,da

REAL :: BE(10000,10000)=0.00

REAL ::  bind,l,m,s1p,s2p,s1n,s2n,salpha,qvalue,BEalpha

REAL :: val1,val2,val3,val4,val5,val6

INTEGER :: Z1,Z2,A1,A2

REAL ::a,b,c,mu,dis,X,Rho,Tudl

BEalpha=28.3

Z2=2
A2=4

OPEN(unit=178,file="outputBE.txt",status="unknown")

DO k=1,10237

READ(178,*)m,l,val1,val2,val3,val4,val5,val6,bind

i=l
j=m
BE(i,j)=-bind

END DO

CLOSE(178)


OPEN(unit=179,file="alpha_input.txt",status="unknown")

DO pz=118,120,1

DO pa=250,350,1

salpha=BE(pz,pa)-BE(pz-2,pa-4)
s1p=BE(pz,pa)-BE(pz-1,pa-1)
s1n=BE(pz,pa)-BE(pz,pa-1)
s2p=BE(pz,pa)-BE(pz-2,pa-2)
s2n=BE(pz,pa)-BE(pz,pa-2)
qvalue=(BE(pz-2,pa-4)+BEalpha)-BE(pz,pa)



a=0.4314
b=-0.4087
c=-25.7725

Z1=pz-Z2
A1=pa-A2

mu=(A1*A2)*(1.0d0/(A1+A2))
dis=(A1**(1.0d0/3.0d0)+ A2**(1.0d0/3.0d0))
X=Z2*Z1*((mu*(1.0d0/qvalue))**(1.0d0/2.0d0))
Rho=(mu*(Z1*Z2)*dis)**(1.0d0/2.0d0)
Tudl=(a*X)+(b*Rho)+ c


WRITE(179,*)pz,pa,pa-pz,qvalue,s1p,s2p,s1n,s2n

END DO

END DO

CLOSE(179)


END PROGRAM create_alpha_input
