PROGRAM create_C_input

INTEGER :: i,j,k,pz,pa,dz,da

REAL :: BE(10000,10000)=0.00

REAL ::  bind,l,m,s1p,s2p,s1n,s2n,salpha,qvalue,BEcluster

INTEGER :: Z1,Z2,A1,A2

REAL ::a,b,c,mu,dis,X,Rho,Tudl


BEcluster=105.67

Z2=6
A2=14


OPEN(unit=178,file="outputBE.txt",status="unknown")

DO k=1,5635

READ(178,'(3f15.6)')l,m,bind

i=l
j=m
BE(i,j)=bind

END DO

CLOSE(178)


OPEN(unit=179,file="C_input.txt",status="unknown")

DO pz=130,144,2

DO pa=290,380,2

qvalue=(BE(pz-6,pa-12)+BEcluster)-BE(pz,pa)


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

WRITE(179,*)pz,pa,pa-pz,qvalue

END DO

END DO

CLOSE(179)


END PROGRAM create_C_input
