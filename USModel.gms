$ontext
Update: August 25, 2017
This is the latest version of the US SDG model developed by Prof. Sachs.

So far, this model includes:
1. Basic core economic model
2. Population dynamics
3. Basic Education dynamics

Following steps are:
1. Include the 


$offtext

*US SDG Model

Sets t /2015,2020,2025,2030,2035,2040,2045,2050/
         tstart(t)
         tend(t)
         time(t);

tstart(t) = yes$(ord(t) eq 1);
tend(t) = yes$(ord(t) eq  card(t));
time(t) = yes$(ord(t) le 2);

Sets g  /male, female/;

set i /0-4,5-9,10-14,15-19,20-24,25-29,30-34,35-39,40-44,45-49,50-54,55-59,60-64,65-69,70-74,75-79,80-84,85-89,90-94,95-99/
        wa(i)
        old(i)
        adult(i);

wa(i) = yes$(ord(i) ge 6 and ord(i) le 13);
adult(i) = yes$((ord(i) ge 6) and (ord(i) le 17));
old(i) = yes$((ord(i) ge 14) and (ord(i) le 17));

parameter workage(i),oldage(i);
workage(i)$(ord(i) le 5) = 0;
workage(i)$(ord(i) gt 5 and ord(i) lt 14) = 1;
workage(i)$(ord(i) ge 14) = 0;
oldage(i)$(ord(i) le 13) = 0;
oldage(i)$(ord(i) ge 14) = 1;

Set occ   ;

Set iter /1*5/;

Sets s sectors

/AG, MIN, UTIL, CON, MAN, WT, RT, TRAN, INFO, FIN, RE, PROF, EDUC, HL, ARTS, ACCOM, OTHSER, GOV, SLF, TOT/

    s1(s)   all but self and total
    s2(s)   all be self, total, and man
    mar(s)  market sectors (all but EDUC HL GOV SLF TOT)
    nmar(s) non-market sectors (EDUC HL GOV SLF TOT)
    gv(s)   government
    pr(s)   private
    tr(s)   traded
    ntr(s)  non-traded
    fd(s)   food
    mg(s)   mining
    el(s)   electricity
    edu(s)  education
    he(s)   health
    opr(s)  other private sector
    snm(s)  all sectors but manufacturing
     ex(s)  extractive sectors (ag and min)
    cons(s) construction
   manu(s)  manufacturing
   nman(s)  non-manufacturing
   cpr(s)   private consumption
     ns(s)  inputs for s;

s1(s) = yes$(ord(s) lt 19);
mar(s) = yes$(ord(s) lt 13 or (ord(s) ge 15 and ord(s) lt 18));
nmar(s) = yes$((ord(s) eq 13) or (ord(s) eq 14) or (ord(s) eq 18));
s2(s) = yes$(ord(s) lt 19 and (ord(s) ne 5));
nman(s) = yes$((ord(s) lt 19) and (ord(s) ne 5) and ((ord(s) ne 13) and (ord(s) ne 14) and (ord(s) ne 18)));
manu(s) = yes$(ord(s) eq 5);

Alias(s1,salias1);

Set ed /lths, hs, sc, ad, bach, mast, pro/
lowed(ed)
meded(ed)
hied(ed);

Alias(ed,ed1);

lowed(ed) = yes$(ord(ed) le 1);
meded(ed) = yes$(ord(ed) ge 2 and ord(ed) le 4);
hied(ed) =  yes$(ord(ed) gt 4);

Parameter EducationTarget(ed)

/
lths   0
hs    .35
sc     0
ad    .10
bach  .35
mast  .10
pro   .10 /;

Parameter Lprod(ed)

/lths  0.68
hs 1.00
sc 1.18
ad 1.30
bach 1.92
mast 2.34
pro 3.60 /;

Parameter Lprodt(t,ed);

Lprodt(t,ed) = Lprod(ed);

Lprodt(t,ed)$(ord(ed) ge 5) = Lprodt(t,ed)*(1+.10)**(ord(t)-1);

Set       occ  ;

Parameter Pop0(i,g)
          Surv(t,i,g)
          EducationAttainmentMale(i,ed)
          EducationAttainmentFemale(i,ed)
          EduOcc(occ,ed)
          Computer(occ)
          IO(s,s)
          EmployOcc(occ,s)
          aL(s,ed)
          MPCw(i)
          MPCa(i)
          VADR(s)
          DIRREQ(s,s);

$CALL GDXXRW usagdx.xlsx Index=Index!a1 trace=0
$GDXIN usagdx.gdx
$LOAD Pop0=D1 Surv=D2 EducationAttainmentMale=D3 EducationAttainmentFemale=D4 Occ=D5 EduOcc=D6 Computer=D7 EmployOcc=D8 IO=D9 aL=D10 MPCw=D11 MPCa=D12 DIRREQ=D13 VADR=D14
$GDXIN


Parameter FERT(i)

/15-19  18.789
20-24   69.608
25-29   110.128
30-34   111.373
35-39   55.158
40-44   11.432
45-49    0.792 /;

** Population Dynamics

Parameter Population(t,i,g), Births(t,g), Populationtotal(t);

Population("2015",i,g) = Pop0(i,g);

Parameter birthgender(g)

/male 0.51
female 0.49/;

Loop(t,
Population(t+1,i+1,g) = Population(t,i,g)*Surv(t,i,g);
Births(t,g) = 5*(1/1000)*birthgender(g)*sum(i,FERT(i)*.5*(Population(t,i,"female")+Population(t+1,i,"female")));
Population(t+1,"0-4",g) = Births(t,g);)  ;

Populationtotal(t) = sum(i,sum(g,Population(t,i,g)));
Parameter Pop(t,g),Poptotal;
Pop(t,g) = sum(i,Population(t,i,g));

Display Population, Births, Populationtotal;

** Education Dynamics

Parameter EducationAttainPop(t,i,ed,g);
EducationAttainPop("2015",i,ed,"male") = EducationAttainmentMale(i,ed);
EducationAttainPop("2015",i,ed,"female") = EducationAttainmentFemale(i,ed);

Parameter EducationAttainment(t,i,ed,g);
EducationAttainment("2015",wa,ed,g) = EducationAttainPop("2015",wa,ed,g)/
         sum(ed1,EducationAttainPop("2015",wa,ed1,g));

Loop(t,
EducationAttainment(t+1,"25-29",ed,g) = EducationTarget(ed););

Loop(t,
Loop(i,EducationAttainment(t+1,i+1,ed,g) = EducationAttainment(t,i,ed,g));
EducationAttainment(t+1,"25-29",ed,g) = EducationTarget(ed););

Display EducationAttainment;

Parameter EducAttainPop(t,i,ed,g);
EducAttainPop(t,i,ed,g) = EducationAttainment(t,i,ed,g)* Population(t,i,g);


Display EducAttainPop;

Parameter Ltot(t,ed), LbyEd(t,i,ed) ;

Ltot(t,ed) = sum(g,sum(wa,EducAttainPop(t,wa,ed,g)));
LbyEd(t,wa,ed) = sum(g,EducAttainPop(t,wa,ed,g));

** School Population

Parameter PSchoolt(t), LSSchoolt(t), USSchoolt(t), LTSchoolt(t), UTSchoolt(t);
Parameter PSchool,LSSchool,USSchool,LTSchool,UTSchool;

PSchoolt(t) = sum(g,Population(t,"5-9",g));
LSSchoolt(t) = sum(g,Population(t,"10-14",g));
USSchoolt(t) = sum(g,Population(t,"15-19",g));
LTSchoolt(t) = sum(g,Population(t,"20-24",g))*.55;
UTSchoolt(t) = sum(g,Population(t,"25-29",g))*.20;

** Labor Force

Scalar dep;
dep = 0.05;

Parameter msh(s)

/AG        0.3
MIN        0.31
UTIL        0.32
CON        0.33
MAN        0.34
WT        0.35
RT        0.36
TRAN        0.37
INFO        0.38
FIN        0.39
RE        0.40
PROF        0.41
EDUC        0.42
HL        0.43
ARTS        0.44
ACCOM        0.45
OTHSER        0.46
GOV        0.47
TOT        0.48  /;


Parameter rg(t);

rg(t) = 0.1;

Parameter INsh(s)

/AG   0
MIN   0
UTIL  0
CON   0.3
MAN   0.5
WT    0
RT    0
TRAN  0
INFO  0
FIN   0
RE    0
PROF  0.2
EDUC  0
HL    0
ARTS  0
ACCOM 0
OTHSER 0
GOV    0 /;

Parameter csh(s)

/AG        0.0086
MIN        0.0142
UTIL        0.0155
CON        0.0422
MAN        0.1171
WT        0.0591
RT        0.0585
TRAN        0.0303
INFO        0.0477
FIN        0.0730
RE        0.1326
PROF        0.1243
EDUC        0.0112
HL        0.0737
ARTS        0.0107
ACCOM        0.0297
OTHSER        0.0226
GOV        0.1288/;

Parameter Ksh(i)

/25-29        0
30-34        0.01419885
35-39        0.03024355
40-44        0.048374061
45-49        0.068861539
50-54        0.092012389
55-59        0.118172849
60-64        0.147734169
65-69        0.181138461
70-74        0.143788761
75-79        0.101583601
80-84        0.053891769/;

Parameter YLesh(ed)

/lths     0.046792198
hs        0.195523398
sc        0.138265588
ad        0.092606436
bach      0.284452047
mast      0.155221889
pro       0.087138446 /;

***** OLG Model

Parameter AI(s,ed);

AI(s,ed) = 0;

Parameter Letot(ed),Letotal(t,ed), Ltie(t,i,ed), Lie(i,ed);
Letotal(t,ed) = sum(wa,sum(g,EducAttainPop(t,wa,ed,g)));
Ltie(t,wa,ed) = sum(g, EducAttainPop(t,wa,ed,g));

Parameter Db;
Db = 0;

Parameter disc, MPCWe(i),MPCAs(i);
disc = 1/(1+.13);
MPCWe(i) = 0;
MPCWe(i)$((ord(i) gt 5) and (ord(i) lt 14)) = (1-disc**(14-ord(i)))/(1-disc**(18-ord(i)));
MPCAs(i) = 0;
MPCAs(i)$((ord(i) gt 5) and (ord(i) lt 18)) = (1-disc)/(1-disc**(18-ord(i)));

Parameter VAT;
VAT = 0;

Positive Variables

Qs(s)
Cs(s)
Gs(s)
Con
Con1                                                                                                                                                                   7
CONie(i,ed)
Conie1(i,ed)
INV
Is(s)
PI
Lse(s,ed)
EFFL(s)
Ms(s)
Rse(s,ed)
Rs(s)
Rob
Mac
Pc(s)
Ps(s)
We(s,ed)
rrate(s)
rrob(s,ed)
GDP
GNP
GNP1
Sie(i,ed)
YLie(i,ed)
YKie(i,ed)
YL
YLalt
YK
Lincome
KN
KNtest
KNie(i,ed)
Sub(s)
GovC
Tx
Wtax ;

Variables

INV1
Sav
Aie(i,ed)
Util
DbN;

** Cost Structures

Parameters cps,cls,cus,clt,cut,chl,cgov;

cps = .05;
cls = .05;
cus = .05;
clt = .05;
cut = .05;
chl = .05;
cgov = .05;

Parameter rho;

rho =-0.5;

** Initial Conditions (2015)

Poptotal = sum(g,Pop("2015",g));
Scalar Ktot0,Ktot;
Ktot0 = 500000;
Ktot = 500000;
Parameter Kie(i,ed);
Ksh("85-89") = 0;
Kie(i,ed) = Ktot*Ksh(i)*YLesh(ed);
Letot(ed) = Letotal("2015",ed);
Lie(wa,ed) = sum(g,EducAttainPop("2015",wa,ed,g));
PSchool = PSchoolt("2015");
LSSchool = LSSchoolt("2015");
USSchool = USSchoolt("2015");
LTSchool = LTSchoolt("2015");
UTSchool = UTSchoolt("2015");
Parameter AI(s,ed),AIs(s,ed);
AI(s1,ed) = 0;
AI(s1,lowed) = 0.5;
AI(s1,meded) = 1.0;
AIs(s,ed) = AI(s,ed);
Parameter AIt(t);
AIt(t) = 2*ord(t);
Parameter Itot;
Itot = 747009;

Equations

Output(s)
Demand(s)
InvPrice
InvestGood
SecInvest(s)
Robot
Machine
Capital
Labor(ed)
Numeraire(s)
Price(s)
Wage(s,ed)
Irate(s)
EffectiveLabor(s)
Iraterob(s,ed)
NationalOutput
KNext
LabY(i,ed)
CapY(i,ed)
AssetY(i,ed)
Consume(i,ed)
LaborIncome
CapitalIncome
NationalIncome
Government
BalBudget
Wtaxrate
CONtotal
PrivCon
SECDemand(s)
AGDemand
KNbyAge(i,ed)
KNbyAge1(i,ed)
KNextTest
PrivSav
GovEduc(s)
GovHealth(s)
GovPublicAd(s)
Utility ;

Output(s1)..            Qs(s1) =e= EFFL(s1)**(1-msh(s1))*Ms(s1)**msh(s1);
Demand(s1)..            Cs(s1)+Is(s1) =e= Qs(s1);
InvPrice..              PI =e= sum(s1,INsh(s1)*Ps(s1));
InvestGood..            INV =e= Itot*PI;
SecInvest(s1)..         Is(s1) =e= (INV/PI)*INsh(s1);
Robot..                 Rob =e= sum(ed,sum(s1,Rse(s1,ed))) ;
Machine..               Mac =e= sum(s1,Ms(s1));
Capital..               Mac + Rob =e= Ktot ;
Labor(ed)..             sum(s1,Lse(s1,ed)) =e= Letot(ed);
Numeraire("MAN")..      Ps("MAN") =e= 1;
Price(nmar)..           Ps(nmar)*Qs(nmar) =e= sum(ed,We("MAN",ed)*Lse(nmar,ed))+rrate("MAN")*(Ms(nmar)+sum(ed,Rse(nmar,ed)));
Wage(s1,ed)..           We(s1,ed) =e= Lprod(ed)*(1-msh(s1))*Ps(s1)*Qs(s1)/EFFL(s1)*(EFFL(s1)/(Lprod(ed)*Lse(s1,ed)+AIs(s1,ed)*Rse(s1,ed)))**(rho+1);
Irate(s1)..             rrate(s1) =e= msh(s1)*Ps(s1)*Qs(s1)/Ms(s1);
EffectiveLabor(s1)..    EFFL(s1) =e= (sum(ed,(Lprod(ed)*Lse(s1,ed)+AIs(s1,ed)*Rse(s1,ed))**(-rho)))**(-1/rho);
Iraterob(s1,ed)..       rrob(s1,ed) =e= AIs(s1,ed)*(1-msh(s1))*Ps(s1)*Qs(s1)/EFFL(s1)*(EFFL(s1)/(Lprod(ed)*Lse(s1,ed)+AIs(s1,ed)*Rse(s1,ed)))**(rho+1);
NationalOutput..        GDP =e= sum(s1,Ps(s1)*Qs(s1));
KNext..                 KN =e= (1-dep)*Ktot + Itot;
LabY(wa,ed)..           YLie(wa,ed) =e= We("MAN",ed)*Lie(wa,ed)*(1-Wtax);
CapY(adult,ed)..        YKie(adult,ed) =e= rrate("MAN")*Kie(adult,ed);
AssetY(adult,ed)..      Aie(adult,ed) =e= (1+rrate("MAN"))*Kie(adult,ed);
Consume(adult,ed)..     CONie(adult,ed) =e= MPCWe(adult)*(YLie(adult,ed)) + MPCAs(adult)*Aie(adult,ed) ;
LaborIncome..           YL =e= sum(ed,We("MAN",ed)*Letot(ed));
CapitalIncome..         YK =e= rrate("MAN")*Ktot;
NationalIncome..        GNP =e= YL + YK;
Government..            sum(nmar,Ps(nmar)*Cs(nmar)) =e= GovC;
BalBudget..             Tx =e= GovC;
Wtaxrate..              Tx =e= Wtax*YL;
CONtotal..              Con1 =e= sum(adult,sum(ed,CONie(adult,ed)));
PrivCon..               Con =e= sum(mar,Ps(mar)*Cs(mar));
SECDemand(mar)..        Cs(mar) =e= Csh(mar)*Cs("MAN")/(Csh("MAN")*Ps(mar));
AGDemand..              GDP =e= Con1 + GovC + INV1;
KNbyAge(i+1,ed)..       KNie(i+1,ed) =e= Kie(i,ed)*(1-dep)+(YLie(i,ed)+YKie(i,ed) - CONie(i,ed))/PI;
KNbyAge1("25-29",ed)..  KNie("25-29",ed) =e= 0;
PrivSav..               Sav =e= sum(i,sum(ed,YLie(i,ed)+YKie(i,ed)-CONie(i,ed)));
KNextTest..             KNtest =e= sum(adult,sum(ed,KNie(adult,ed)));
GovEduc("Educ")..       Cs("Educ") =e= cps*(PSchool+cls*LSSchool+cus*USSchool+clt*LTSchool+cut*UTSchool);
GovHealth("HL")..       Cs("HL") =e= chl*Poptotal;
GovPublicAd("GOV")..    Cs("GOV") =e= cgov*Poptotal;
Utility..               Util =e= sum(s1,Csh(s1)*log(Cs(s1)));


model USOLG /all/;

Lse.lo(s,ed) = .01;
Ms.lo(s) = .0001;
Cs.lo(s) = .0001;
Rse.lo(s,ed) = .00001;
Ps.lo(s) = .0001;
We.lo(s,ed) = .0001;
rrate.lo(s) = .0001;
Qs.lo(s) = .0001;
KN.lo = .0001;
EFFL.lo(s) = .01;
PI.lo = .1;

Parameter rt(t),Kt(t),Kttest(t),Ktie(t,i,ed),Kie(i,ed),Stie(t,i,ed),GDPt(t),GNPt(t),Wedt(t,ed),Lset(t,s,ed),EFFLt(t,s);
Parameter Cont(t),GovCt(t),Invt(t),Txt(t),Rset(t,s,ed),Mst(t,s), Con1t(t);
Parameter YLt(t), YKt(t);
Parameter INV1t(t);
Parameter Mact(t),Robt(t);

Loop(time,
Lprod(ed) = Lprodt(time,ed);
Letot(ed) = Letotal(time,ed);
Lie(wa,ed) = Ltie(time,wa,ed);
Loop(iter,Solve USOLG maximizing Util using dnlp;
Itot = INV1.L/PI.L;);
Wedt(time,ed) = We.L("MAN",ed);
AIs(s1,ed) = AI(s1,ed)* AIt(time);
Lset(time,s1,ed) = Lse.L(s1,ed);
EFFLt(time,s1) = EFFL.L(s1);
rt(time) = rrate.L("MAN");
Kt(time) = Ktot;
Kttest(time) = sum(i,sum(ed,Kie(i,ed)));
Ktie(time,i,ed) = Kie(i,ed);
Kie(i,ed) = KNie.L(i,ed);
GDPt(time) = GDP.L;
GNPt(time) = GNP.L;
Cont(time) = Con.L;
GovCt(time) = GovC.L;
Invt(time) = Inv.L;
Inv1t(time) = Inv1.L;
Txt(time) = Tx.L;
YLt(time) = YL.L;
YKt(time) = YK.L;
Con1t(time) = Con1.L;
Rset(time,s1,ed) = Rse.L(s1,ed);
Mst(time,s1) = Ms.L(s1);
Mact(time) = Mac.L;
Robt(time) = Rob.L;
Ktot = KN.L;);

display Cont, Con1t;

display Mact, Robt;

display INVt,GovCt,Cont,GDPt;

display GovCt,Txt;

display GNPt,GDPt;

display Kt, Rset, Mst;

display Kt, Kttest;

display KN.L, KNtest.L;

display Con.L, Con1.L;

display INV.L, INV1.L, Sav.L;

display Wedt;









