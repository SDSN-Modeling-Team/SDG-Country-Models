$title US SDG Model
$stitle Sustainable Development Solutions Network
$stitle Last Updated: September 1, 2017




************
* Settings *
************

$ontext
This section contains the set and parameter declaration in addition to the
GDX file creation and loading.
$offtext

* Set Declaration
*****************

Sets     t       Time
                 tstart(t)       First Period
                 tend(t)         Last Period
                 time(t)         Test Periods
         g       Gender
         i       Generation
                 wa(i)           Working Age
                 old(i)          Old Age
                 adult(i)        Adult Age
         occ     Occupations
         s       Sectors
                 s1(s)           all but self and total
                 s2(s)           all be self total and man
                 mar(s)          market sectors (all but EDUC HL GOV SLF TOT)
                 nmar(s)         non-market sectors (EDUC HL GOV SLF TOT)
                 gv(s)           government
                 pr(s)           private
                 tr(s)           traded
                 ntr(s)          non-traded
                 fd(s)           food
                 mg(s)           mining
                 el(s)           electricity
                 edu(s)          education
                 he(s)           health
                 opr(s)          other private sector
                 snm(s)          all sectors but manufacturing
                 ex(s)           extractive sectors (ag and min)
                 cons(s)         construction
                 manu(s)         manufacturing
                 nman(s)         non-manufacturing market sectors
                 cpr(s)          private consumption
                 ns(s)           inputs for s
         iter    Iteration
         ed      Education
                 lowed(ed)       Lower Education
                 meded(ed)       Medium Education
                 hied(ed)        High Education;

Alias    (s1,salias1)
         (ed,ed1);

* Parameter Declaration
***********************

* The next list of parameters comes from the GDX file:

Parameter        EducTarget(ed)                  SDG Education Target
                 Lprod0(ed)                      Initial Productivity by Ed Level
                 Pop0(i,g)                       Initial Population
                 Surv(t,i,g)                     Survival Rate
                 Educ0(i,g,ed)                   Initial Education Attainment
                 EduOcc(occ,ed)                  Education by Occupation
                 Computer(occ)                   Probability of Automation
                 IO(s,s)                         Input-Output Table
                 EmployOcc(occ,s)                Occupations by Sector
                 aL(s,ed)                        Education by Sector
                 MPCw(i)                         Marginal Propensity to Consume Wage
                 MPCa(i)                         Marginal Propensity to Consume Assets
                 VADR(s)                         Value Added Direct Requirement
                 DIRREQ(s,s)                     Direct Requirement
                 FERT(t,i)                       Projected Fertility Rates
                 birthgender(g)                  Gender distribution at birth
                 msh(s)                          Machine Share in Production
                 INsh(s)                         Investment Good Share
                 csh(s)                          Consumption Share in Utility Function
                 Ksh(i)                          Wealth Distribution by Generation
                 YLesh(ed)                       Wealth Distribution by Educ Level;

* GDX Declaration
*****************

*$CALL GDXXRW usagdx.xlsx Index=Index!a1 trace=0
$GDXIN usagdx.gdx
$LOAD  t=S1 g=S2 i=S3 occ=S4 s=S5 ed=S6 iter=S7
$LOAD  Pop0=D1 Surv=D2 FERT=D3 birthgender=D4 Educ0=D5 EducTarget=D6
$LOAD  Lprod0=D7 EduOcc=D8 Computer=D9 EmployOcc=D10 IO=D11 aL=D12
$LOAD  MPCw=D13 MPCa=D14 DIRREQ=D15 VADR=D16 msh=D17 INsh=D18
$LOAD  csh=D19 Ksh=D20 YLesh=D21
$GDXIN

* Subset modification
*********************

tstart(t)  = yes$(ord(t) eq 1);
tend(t)    = yes$(ord(t) eq  card(t));
time(t)    = yes$(ord(t) le 2);

wa(i)      = yes$(ord(i) ge 6 and ord(i) le 13);
adult(i)   = yes$((ord(i) ge 6) and (ord(i) le 17));
old(i)     = yes$((ord(i) ge 14) and (ord(i) le 17));

s1(s)      = yes$(ord(s) lt 19);
mar(s)     = yes$(ord(s) lt 13 or (ord(s) ge 15 and ord(s) lt 18));
nmar(s)    = yes$((ord(s) eq 13) or (ord(s) eq 14) or (ord(s) eq 18));
s2(s)      = yes$(ord(s) lt 19 and (ord(s) ne 5));
nman(s)    = yes$((ord(s) lt 19) and (ord(s) ne 5) and ((ord(s) ne 13)
             and (ord(s) ne 14) and (ord(s) ne 18)));
manu(s)    = yes$(ord(s) eq 5);

lowed(ed)  = yes$(ord(ed) le 1);
meded(ed)  = yes$(ord(ed) ge 2 and ord(ed) le 4);
hied(ed)   = yes$(ord(ed) gt 4);

*The next list of parameters are produced within the model:

parameter        workage(i)                      Working Age Dummies
                 oldage(i)                       Old Age Dummies
                 Lprodt(t,ed)                    Lprod Time Based;

workage(i)$(ord(i) le 5) = 0;
workage(i)$(ord(i) gt 5 and ord(i) lt 14) = 1;
workage(i)$(ord(i) ge 14) = 0;
oldage(i)$(ord(i) le 13) = 0;
oldage(i)$(ord(i) ge 14) = 1;

Lprodt(t,ed) = Lprod0(ed);
Lprodt(t,ed)$(ord(ed) ge 5) = Lprodt(t,ed)*(1+.10)**(ord(t)-1);

***********************
* Population Dynamics *
***********************

* Parameter Declaration
***********************

Parameter        Population(t,i,g)       Population by gender and generation at time t
                 Births(t,g)             Total Births by gender at time t
                 Populationtotal(t)      Total Population at time t
                 Pop(t,g)                Total population by gender at time t
                 Poptotal                Population Total updated each loop of t;

* Dynamics
**********

Population("2015",i,g) = Pop0(i,g);

Loop(t,
  Population(t+1,i+1,g) = Population(t,i,g)*Surv(t,i,g);
  Births(t,g) = 5*(1/1000)*birthgender(g)*sum(i,FERT(t,i)*.5*(Population(t,i,"female")+Population(t+1,i,"female")));
  Population(t+1,"0-4",g) = Births(t,g);
);

Populationtotal(t) = sum(i,sum(g,Population(t,i,g)));
Pop(t,g) = sum(i,Population(t,i,g));
Poptotal = sum(g,Pop("2015",g));

Display Population, Births, Populationtotal;

**********************
* Education Dynamics *
**********************

* Parameter Declaration
**********************

Parameter        EducPop(t,i,ed,g)    Population in each Educ Level
                 EducRatio(t,i,ed,g)  % of Population at each Educ Level
                 Ltot(t,ed)           Labor Force by Education Level
                 LbyAge(t,i,ed)       Labor Force by Educ Level and Generation;
* Dynamics
**********

EducPop("2015",i,ed,g) = Educ0(i,g,ed);
EducRatio("2015",wa,ed,g) = EducPop("2015",wa,ed,g)/sum(ed1,EducPop("2015",wa,ed1,g));

Loop(t,
  EducRatio(t+1,"25-29",ed,g) = EducTarget(ed);
);

Loop(t,
  Loop(i,
    EducRatio(t+1,i+1,ed,g) = EducRatio(t,i,ed,g)
  );
  EducRatio(t+1,"25-29",ed,g) = EducTarget(ed);
);

EducPop(t,i,ed,g) = EducRatio(t,i,ed,g)*Population(t,i,g);
Ltot(t,ed) = sum(g,sum(wa,EducPop(t,wa,ed,g)));
LbyAge(t,wa,ed) = sum(g,EducPop(t,wa,ed,g));

Display EducRatio, EducPop;

* School Population
*******************

Parameter        PSchoolt(t)    Primary School Population
                 LSSchoolt(t)   Lower Secondary Population
                 USSchoolt(t)   Upper Secondary Population
                 LTSchoolt(t)   Lower Tertiary Population
                 UTSchoolt(t)   Upper Tertiary Population
                 PSchool0        PS Initial Value
                 LSSchool0       LS Initial Value
                 USSchool0       US Initial Value
                 LTSchool0       LT Initial Value
                 UTSchool0       UT Initial Value;

PSchoolt(t)  = sum(g,Population(t,"5-9",g));
LSSchoolt(t) = sum(g,Population(t,"10-14",g));
USSchoolt(t) = sum(g,Population(t,"15-19",g));
LTSchoolt(t) = sum(g,Population(t,"20-24",g))*.55;
UTSchoolt(t) = sum(g,Population(t,"25-29",g))*.20;

***********************
* Core Economic Model *
***********************

* Parameter and Scalar Declaration
**********************************

parameter        Letot(ed)      Initial Working Age Population at each Education Level
                 Letotal(t,ed)  Letot in each time period
                 Ltie(t,i,ed)   WA Pop at each educ and generation in each time period
                 Lie(i,ed)      Initial Value for Ltie
                 MPCWe(i)       Alternative MPC from wage
                 MPCAs(i)       Alternative MPC from asset;

Scalar   dep             Depreciation Rate of Capital
         disc            Discount Rate;

dep = 0.05;
disc = 1/(1+.13);

Letotal(t,ed) = sum(wa,sum(g,EducPop(t,wa,ed,g)));
Ltie(t,wa,ed) = sum(g, EducPop(t,wa,ed,g));

MPCWe(i) = 0;
MPCWe(i)$((ord(i) gt 5) and (ord(i) lt 14)) = (1-disc**(14-ord(i)))/(1-disc**(18-ord(i)));
MPCAs(i) = 0;
MPCAs(i)$((ord(i) gt 5) and (ord(i) lt 18)) = (1-disc)/(1-disc**(18-ord(i)));

$ontext
The Marginal Propensity used in the consumption equation is not the one derived
from the model, but the one constructed in this section. Is this a placeholder?
$offtext

Positive Variables

Qs(s)            Sector Output
Cs(s)            Sector Consumption
Con              Total Private Consumption
Con1             Total Consumption                                                                                                                                                      7
CONie(i,ed)      Consumption by educ level and generation
INV              Total Investment Quantity
Is(s)            Investment by Sector (Quantity)
PI               Investment Good Price
Lse(s,ed)        Labor demand by educ level and sector
EFFL(s)          Effective Labor
Ms(s)            Machine demand by sector
Rse(s,ed)        Robots demand by sector and education
Rob              Total robot demand
Mac              Total machine demand
Ps(s)            Price of production good by sector
We(s,ed)         Wage by sector and education level
rrate(s)         Rate of return of machines
rrob(s,ed)       Rate of return olf robots by type of robot (educ levels)
GDP              Gross Domestic Product
GNP              Gross National Product
YLie(i,ed)       After Tax Labor Income by educ level and generation
YKie(i,ed)       Capital Income by educ level and generation
YL               Total Labor Income
YK               Total Capital Income
KN               Next Period Capital Stock
KNtest           Next Period Capital Stock Check
KNie(i,ed)       Next Period Capital Stock by generation and educ level
GovC             Government Consumption
Tx               Total Tax Revenue
Wtax             Labor Income Tax Rate;

Variables

INV1             Total Investment Check from GDP equation
Sav              Private Savings
Aie(i,ed)        Assets by generation and educ level
Util             Utility Function;

** Cost Structures

Parameters cps,cls,cus,clt,cut,chl,cgov Per capita costs of government services;

cps  = .05;
cls  = .05;
cus  = .05;
clt  = .05;
cut  = .05;
chl  = .05;
cgov = .05;

Parameter rho Elasticity parameter of substitution between robots and human labor;

rho =-0.5;

** Initial Conditions (2015)

Scalar Ktot0,Ktot;
Ktot0 = 500000;
Ktot = 500000;
Parameter Kie(i,ed);
Ksh("85-89") = 0;

Kie(i,ed) = Ktot*Ksh(i)*YLesh(ed);
Letot(ed) = Letotal("2015",ed);
Lie(wa,ed) = sum(g,EducPop("2015",wa,ed,g));
PSchool0 = PSchoolt("2015");
LSSchool0 = LSSchoolt("2015");
USSchool0 = USSchoolt("2015");
LTSchool0 = LTSchoolt("2015");
UTSchool0 = UTSchoolt("2015");

Parameter        AI(s,ed)        Initial Condition for AI
                 AIs(s,ed)
                 AIt(t);
AI(s1,ed) = 0;
AI(s1,lowed) = 0.5;
AI(s1,meded) = 1.0;
AIs(s,ed) = AI(s,ed);
AIt(t) = 2*ord(t);

Parameter Itot Total Investment;
Itot = 747009;

Equations

Output(s)                Output by Sector
Demand(s)                Demand (Consumption + Investment) by Sector
InvPrice                 Price of Investment Good
InvestGood               Investment Good Quantity
SecInvest(s)             Investment Quantity by Sector
Robot                    Robots employed by sector and skill level
Machine                  Machine capital employed by sector
Capital                  Total capital employed
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
Wage(s1,ed)..           We(s1,ed) =e= Lprod0(ed)*(1-msh(s1))*Ps(s1)*Qs(s1)/EFFL(s1)*(EFFL(s1)/(Lprod0(ed)*Lse(s1,ed)+AIs(s1,ed)*Rse(s1,ed)))**(rho+1);
Irate(s1)..             rrate(s1) =e= msh(s1)*Ps(s1)*Qs(s1)/Ms(s1);
EffectiveLabor(s1)..    EFFL(s1) =e= (sum(ed,(Lprod0(ed)*Lse(s1,ed)+AIs(s1,ed)*Rse(s1,ed))**(-rho)))**(-1/rho);
Iraterob(s1,ed)..       rrob(s1,ed) =e= AIs(s1,ed)*(1-msh(s1))*Ps(s1)*Qs(s1)/EFFL(s1)*(EFFL(s1)/(Lprod0(ed)*Lse(s1,ed)+AIs(s1,ed)*Rse(s1,ed)))**(rho+1);
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
GovEduc("Educ")..       Cs("Educ") =e= cps*(PSchool0+cls*LSSchool0+cus*USSchool0+clt*LTSchool0+cut*UTSchool0);
GovHealth("HL")..       Cs("HL") =e= chl*Poptotal;
GovPublicAd("GOV")..    Cs("GOV") =e= cgov*Poptotal;
Utility..               Util =e= sum(s1,Csh(s1)*log(Cs(s1)));


model USOLG /all/;

**Initial lower bounds for model solution

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
  Lprod0(ed) = Lprodt(time,ed);
  Letot(ed) = Letotal(time,ed);
  Lie(wa,ed) = Ltie(time,wa,ed);
  Poptotal = PopulationTotal(time);
  Loop(iter,Solve USOLG maximizing Util using dnlp;
    Itot = INV1.L/PI.L;
  );
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
  Ktot = KN.L;
);

display Cont, Con1t;

display Mact, Robt;

display INVt, INV1t,GovCt,Cont,GDPt;

display GovCt,Txt;

display GNPt,GDPt;

display Kt, Rset, Mst;

display Kt, Kttest;

display KN.L, KNtest.L;

display Con.L, Con1.L;

display INV.L, INV1.L, Sav.L;

display Wedt;
