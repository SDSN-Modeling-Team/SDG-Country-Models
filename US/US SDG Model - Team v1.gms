$title US SDG Model
$stitle Sustainable Development Solutions Network
$stitle Last Updated: October 20, 2017

************
* Settings *
************

$ontext
This section contains the set and parameter declaration in addition to the
GDX file creation and loading.
$offtext

* Set and Subset Declaration
****************************

Sets     t                       Time
                 tstart(t)       First Period
                 tend(t)         Last Period
                 time(t)         Test Periods
         g                       Gender
         i                       Generation
                 wa(i)           Working Age
                 old(i)          Old Age
                 adult(i)        Adult Age
         occ                     Occupations
         s                       Sectors
                 s1(s)           all but self and total
                 mar(s)          market sectors (all but EDUC HL GOV SLF TOT)
                 nmar(s)         non-market sectors (EDUC HL GOV SLF TOT)
                 tr(s)           internationally tradable
                 ntr(s)          not internationally tradable
                 manu(s)         manufacturing sector (used as numeraire for prices)
         iter                    Iteration
         ed                      Education
                 lowed(ed)       Lower Education
                 meded(ed)       Medium Education
                 hied(ed)        High Education;

Alias    (s1,salias1)
         (ed,ed1);

* GDX Parameter Declaration
***************************

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

* GDX Parameter Definition
**************************

*$CALL GDXXRW usagdx.xlsx Index=Index!a1 trace=0
$GDXIN usagdx.gdx
$LOAD  t=S1 g=S2 i=S3 occ=S4 s=S5 ed=S6 iter=S7
$LOAD  Pop0=D1 Surv=D2 FERT=D3 birthgender=D4 Educ0=D5 EducTarget=D6
$LOAD  Lprod0=D7 EduOcc=D8 Computer=D9 EmployOcc=D10 IO=D11 aL=D12
$LOAD  MPCw=D13 MPCa=D14 DIRREQ=D15 VADR=D16 msh=D17 INsh=D18
$LOAD  csh=D19 Ksh=D20 YLesh=D21
$GDXIN

* Subset Definition
*******************

tstart(t)  = yes$(ord(t) eq 1);
tend(t)    = yes$(ord(t) eq  card(t));
time(t)    = yes$(ord(t) le 2);

wa(i)      = yes$(ord(i) ge 6 and ord(i) le 13);
adult(i)   = yes$((ord(i) ge 6) and (ord(i) le 17));
old(i)     = yes$((ord(i) ge 14) and (ord(i) le 17));

s1(s)      = yes$(ord(s) lt 19);
mar(s)     = yes$(ord(s) lt 13 or (ord(s) ge 15 and ord(s) lt 18));
nmar(s)    = yes$((ord(s) eq 13) or (ord(s) eq 14) or (ord(s) eq 18));
manu(s)    = yes$(ord(s) eq 5);

lowed(ed)  = yes$(ord(ed) le 1);
meded(ed)  = yes$(ord(ed) ge 2 and ord(ed) le 4);
hied(ed)   = yes$(ord(ed) gt 4);


* Model Parameter Declarations and Definitions
**********************************************
*The next list of parameters and scalars are produced within the model itself:

Parameters       Population(t,i,g)       Population by gender and generation at time t
                 Births(t,g)             Total Births by gender at time t
                 Populationtotal(t)      Total Population at time t
                 Pop(t,g)                Total population by gender at time t
                 Poptotal                Population Total updated each loop of t
                 EducPop(t,i,ed,g)       Population in each Educ Level
                 EducRatio(t,i,ed,g)     % of Population at each Educ Level
                 Ltot(t,ed)              Labor Force by Education Level
                 LbyAge(t,i,ed)          Labor Force by Educ Level and Generation
                 Lprodt(t,ed)            Lprod Time Based
                 PSchoolt(t)             Primary School Population at time t
                 LSSchoolt(t)            Lower Secondary Population at time t
                 USSchoolt(t)            Upper Secondary Population at time t
                 LTSchoolt(t)            Lower Tertiary Population at time t
                 UTSchoolt(t)            Upper Tertiary Population at time t
                 PSchool0                PS Initial Value
                 LSSchool0               LS Initial Value
                 USSchool0               US Initial Value
                 LTSchool0               LT Initial Value
                 UTSchool0               UT Initial Value
                 Letot(ed)               Initial Working Age Population at each Education Level
                 Letotal(t,ed)           Letot in each time period
                 Ltie(t,i,ed)            WA Pop at each educ and generation in each time period
                 Lie(i,ed)               Initial Value for Ltie
                 MPCWe(i)                Alternative MPC from wage
                 MPCAs(i)                Alternative MPC from asset
                 AI(s,ed)                Initial Condition for AI
                 AIs(s,ed)               AI productivity by sector and education level
                 AIt(t)                  AI productivity rate of increase
                 Kie(i,ed)               Capital income by age and education level
                 cps                     Per Capita cost of primary education services
                 cls                     Per Capita cost of lower secondary education services
                 cus                     Per Capita cost of upper secondary education services
                 clt                     Per Capita cost of lower tertiary education services
                 cut                     Per capita cost of upper tertiary education services
                 chl                     Per capita cost of health services
                 cgov                    Per capita cost of government services
                 rho                     Elasticity parameter of substitution between robots and human labor;

Scalars          dep                     Depreciation Rate of Capital
                 disc                    Discount Rate
                 Ktot0                   Initial capital Stock
                 Ktot                    Total capital Stock
                 Itot;

***********************
* Population Dynamics *
***********************

Population("2015",i,g)    = Pop0(i,g);

Loop(t,
  Population(t+1,i+1,g)   = Population(t,i,g)*Surv(t,i,g);
  Births(t,g) = 5*(1/1000)*birthgender(g)*sum(i,FERT(t,i)*.5*(Population(t,i,"female")+Population(t+1,i,"female")));
  Population(t+1,"0-4",g) = Births(t,g);
);

Populationtotal(t)       = sum(i,sum(g,Population(t,i,g)));
Pop(t,g)                 = sum(i,Population(t,i,g));
Poptotal                 = sum(g,Pop("2015",g));

**********************
* Education Dynamics *
**********************

EducPop("2015",i,ed,g)        = Educ0(i,g,ed);
EducRatio("2015",wa,ed,g)     = EducPop("2015",wa,ed,g)/sum(ed1,EducPop("2015",wa,ed1,g));

Loop(t,
  EducRatio(t+1,"25-29",ed,g) = EducTarget(ed);
);

Loop(t,
  Loop(i,
    EducRatio(t+1,i+1,ed,g)   = EducRatio(t,i,ed,g)
  );
  EducRatio(t+1,"25-29",ed,g) = EducTarget(ed);
);

EducPop(t,i,ed,g)        = EducRatio(t,i,ed,g)*Population(t,i,g);
Ltot(t,ed)               = sum(g,sum(wa,EducPop(t,wa,ed,g)));
LbyAge(t,wa,ed)          = sum(g,EducPop(t,wa,ed,g));

*Labor Productivity
*******************
Lprodt(t,ed)     = Lprod0(ed);
Lprodt(t,ed)$(ord(ed) ge 5)      = Lprodt(t,ed)*(1+.10)**(ord(t)-1);

* School Population
*******************

PSchoolt(t)  = sum(g,Population(t,"5-9",g));
LSSchoolt(t) = sum(g,Population(t,"10-14",g));
USSchoolt(t) = sum(g,Population(t,"15-19",g));
LTSchoolt(t) = sum(g,Population(t,"20-24",g))*sum(ed,yes$(ord(EducTarget) ge 3);
UTSchoolt(t) = sum(g,Population(t,"25-29",g))*sum(ed,yes$(ord(EducTarget) ge 4);

*Cost Structures of Government Services
***************************************

cps  = .05;
cls  = .05;
cus  = .05;
clt  = .05;
cut  = .05;
chl  = .05;
cgov = .05;

***********************
* Core Economic Model *
***********************
rho = -0.5;
dep  = 0.05;
disc = 1/(1+.13);

AI(s1,ed)        = 0;
AI(s1,lowed)     = 0.5;
AI(s1,meded)     = 1.0;
AIs(s,ed)        = AI(s,ed);
AIt(t)           = 2*ord(t);

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

*Starting Conditions (2015)
***************************

Ktot0    = 500000;
Ktot     = 500000;
Itot     = 747009;

Ksh("85-89")     = 0;
Kie(i,ed)        = Ktot*Ksh(i)*YLesh(ed);
Letot(ed)        = Letotal("2015",ed);
Lie(wa,ed)       = sum(g,EducPop("2015",wa,ed,g));
PSchool0         = PSchoolt("2015");
LSSchool0        = LSSchoolt("2015");
USSchool0        = USSchoolt("2015");
LTSchool0        = LTSchoolt("2015");
UTSchool0        = UTSchoolt("2015");

*Variable Declaration
*********************

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
Wse(s,ed)         Wage by sector and education level
MRs(s)           Rate of return of machines
RRse(s,ed)       Rate of return of robots by type of robot (educ levels)
IRse(s,ed)       Average rate of return for capital
GDP              Gross Domestic Product
GNP              Gross National Product
YLie(i,ed)       Net Labor Income by educ level and generation (after tax)
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


*Model Equations
****************

Equations

PRODEQ1(s)               Output by Sector
PRODEQ2(s)               Effective Labour

MARKEQ1(s)               Demand (Consumption + Investment) by Sector
MARKEQ2                  Total capital employed
MARKEQ3(ed)              Labor employed by education level

INVEQ1                   Total Quantity of Investment Goods
INVEQ2                   Price of Investment Good
INVEQ3(s)                Quantity of Investment by Sector
INVEQ4                   Next period's initial capital stock

PRICEQ1(s)               Price of manufacturing good
PRICEQ2(s)               Price of non-market goods

FACTEQ1(s,ed)            Wage by sector and education level
FACTEQ2(s)               Rate of Return on Machines
FACTEQ3(s,ed)            Rate of Return on Robots
FACTEQ4(s,ed)            Average Rate of Return

NATEQ1                   Calculation of GDP by summing sectors
NATEQ2(i,ed)             Capital income by generation and education level
NATEQ3                   Total labor income
NATEQ4                   Total capital income
NATEQ5                   Total capital and labor income
NATEQ6                   Total expenditure on market goods
NATEQ7(s)                Demand for market sector goods

SAVEQ1(i,ed)             Stock of capital assets by generation and education level
SAVEQ2(i,ed)             Labor income by generation and education level
SAVEQ3(i,ed)             Level of consumption by generation and education level
SAVEQ4                   total household consumption (test)
SAVEQ5                   C + I + G GDP Calculation
SAVEQ6                   Total private Saving

KNbyAge(i,ed)            Capital ownership in next period
KNbyAge1(i,ed)           Capital ownership of generation entering workforce = 0
KNextTest                sum of all generations' assets

GOVEQ1(s)                Government Education Expenditure
GOVEQ2(s)                Government Health Expenditure
GOVEQ3(s)                Government Public Administration Expenditure
GOVEQ4                   Total gov't expenditure on non-market goods
GOVEQ5                   Labor income tax rate
GOVEQ6                   Balance Government expenditure with tax revenue

Utility                  Total Utility;

*Production Block
PRODEQ1(s1)..           Qs(s1) =e= EFFL(s1)**(1-msh(s1))*Ms(s1)**msh(s1);
PRODEQ2(s1)..           EFFL(s1) =e= (sum(ed,(Lprod0(ed)*Lse(s1,ed)+AIs(s1,ed)*Rse(s1,ed))**(-rho)))**(-1/rho);

*Market Clearing
MARKEQ1(s1)..           Cs(s1)+Is(s1) =e= Qs(s1);
MARKEQ2..               KTOT =e= sum(ed,sum(s1,Rse(s1,ed))) + sum(s1,Ms(s1));
MARKEQ3(ed)..           sum(s1,Lse(s1,ed)) =e= Letot(ed);

*Factor Markets
FACTEQ1(s1,ed)..        Wse(s1,ed) =e= (Lprod0(ed)*(1-msh(s1))*Ps(s1)*Qs(s1)/EFFL(s1)) * (EFFL(s1)/(Lprod0(ed)*Lse(s1,ed)+AIs(s1,ed)*Rse(s1,ed)))**(rho+1);
FACTEQ2(s1)..           MRs(s1) =e= msh(s1)*Ps(s1)*Qs(s1)/Ms(s1);
FACTEQ3(s1,ed)..        RRse(s1,ed) =e= (AIs(s1,ed)*(1-msh(s1))*Ps(s1)*Qs(s1)/EFFL(s1)) * (EFFL(s1)/(Lprod0(ed)*Lse(s1,ed)+AIs(s1,ed)*Rse(s1,ed)))**(rho+1);
FACTEQ4(s1,ed)          IRse(s1,ed) =e= (MRs(s1) * sum(s1,(Ms(s1))) + RRse(s1,ed) * sum(s1,sum(ed,(Rse(s1,ed)))) / sum(s1,(Ms(s1) + sum(ed,(Rse(s1,ed)))));

*Investment / Savings Sector [Financial Markets]
INVEQ1..                INV =e= Itot*PI;
INVEQ2..                PI =e= sum(s1,INsh(s1)*Ps(s1));
INVEQ3(s1)..            Is(s1) =e= (INV/PI)*INsh(s1);
INVEQ4..                KN =e= (1-dep)*Ktot + Itot;
*Investment dynamic seems incorrect. Savings today (AGDemand) are transformed into
*investments tomorrow, which are transformed into capital the period after that.
*The proposed change has InvestGood eliminated, INV1 renamed INV, and Knext
*including INV/PI  instead of Itot.

*Savings
SAVEQ1(adult,ed)..      Aie(adult,ed) =e= (1+IRse(manu))*Kie(adult,ed);
SAVEQ2(wa,ed)..         YLie(wa,ed) =e= Wse(manu,ed)*Lie(wa,ed)*(1-Wtax);
SAVEQ3(adult,ed)..      CONie(adult,ed) =e= MPCWe(adult)*(YLie(adult,ed)) + MPCAs(adult)*Aie(adult,ed) ;
SAVEQ4..                Con1 =e= sum(adult,sum(ed,CONie(adult,ed)));
SAVEQ5..                GDP =e= Con1 + GovC + INV1;
*Savings should = INV1, so this is a test equation?
SAVEQ6..                Sav =e= sum(i,sum(ed,YLie(i,ed)+YKie(i,ed)-CONie(i,ed)));

*Prices
PRICEQ1(manu)..          Ps(manu) =e= 1;
PRICEQ2(nmar)..          Ps(nmar)*Qs(nmar) =e= sum(ed,Wse(manu,ed)*Lse(nmar,ed))+IRse(manu)*(Ms(nmar)+sum(ed,Rse(nmar,ed)));

*National Accounts
NATEQ1..                GDP =e= sum(s1,Ps(s1)*Qs(s1));
*rrate is incorrect; should be total return or weighted return between machines, robots
NATEQ2(adult,ed)..      YKie(adult,ed) =e= rrate(manu)*Kie(adult,ed);
NATEQ3..                YL =e= sum(ed,Wse(manu,ed)*Letot(ed));
*rrate is incorrect; should be total return or weighted return between machines, robots
NATEQ4..                YK =e= IRse(manu)*Ktot;
NATEQ5..                GNP =e= YL + YK;
*Irrelevant variable
NATEQ6..                Con =e= sum(mar,Ps(mar)*Cs(mar));
*Product of first order condition of utility problem
NATEQ7(mar)..           Cs(mar) =e= Csh(mar)*Cs(manu)/(Csh(manu)*Ps(mar));

*Government
GOVEQ1("Educ")..        Cs("Educ") =e= cps*(PSchool0+cls*LSSchool0+cus*USSchool0+clt*LTSchool0+cut*UTSchool0);
GOVEQ2("HL")..          Cs("HL") =e= chl*Poptotal;
GOVEQ3("GOV")..         Cs("GOV") =e= cgov*Poptotal;
GOVEQ4..                sum(nmar,Ps(nmar)*Cs(nmar)) =e= GovC;
GOVEQ5..                Tx =e= Wtax*YL;
GOVEQ6..                Tx =e= GovC;

*Test Equations


KNbyAge(i+1,ed)..       KNie(i+1,ed) =e= Kie(i,ed)*(1-dep)+(YLie(i,ed)+YKie(i,ed) - CONie(i,ed))/PI;
KNbyAge1("25-29",ed)..  KNie("25-29",ed) =e= 0;
KNextTest..             KNtest =e= sum(adult,sum(ed,KNie(adult,ed)));


*Objective Function
Utility..               Util =e= sum(s1,Csh(s1)*log(Cs(s1)));

model USOLG /all/;

Lse.lo(s,ed)     = .01;
Ms.lo(s)         = .0001;
Cs.lo(s)         = .0001;
Rse.lo(s,ed)     = .00001;
Ps.lo(s)         = .0001;
Wse.lo(s,ed)      = .0001;
rrate.lo(s)      = .0001;
Qs.lo(s)         = .0001;
KN.lo            = .0001;
EFFL.lo(s)       = .01;
PI.lo            = .1;

*Results Calculation
********************

Parameter rt(t),Kt(t),Kttest(t),Ktie(t,i,ed),Kie(i,ed),Stie(t,i,ed),GDPt(t),
         GNPt(t),Wedt(t,ed),Lset(t,s,ed),EFFLt(t,s), Cont(t),GovCt(t),Invt(t),
         Txt(t),Rset(t,s,ed),Mst(t,s), Con1t(t), YLt(t), YKt(t),INV1t(t),
         Mact(t),Robt(t);

Loop(time,
  Lprod0(ed)             = Lprodt(time,ed);
  Letot(ed)              = Letotal(time,ed);
  Lie(wa,ed)             = Ltie(time,wa,ed);
  Poptotal               = PopulationTotal(time);
  Loop(iter,
    Solve USOLG maximizing Util using dnlp;
    Itot                 = INV1.L/PI.L;
  );
  Wedt(time,ed)          = Wse.L(manu,ed);
  AIs(s1,ed)             = AI(s1,ed)* AIt(time);
  Lset(time,s1,ed)       = Lse.L(s1,ed);
  EFFLt(time,s1)         = EFFL.L(s1);
  rt(time)               = rrate.L(manu);
  Kt(time)               = Ktot;
  Kttest(time)           = sum(i,sum(ed,Kie(i,ed)));
  Ktie(time,i,ed)        = Kie(i,ed);
  Kie(i,ed)              = KNie.L(i,ed);
  GDPt(time)             = GDP.L;
  GNPt(time)             = GNP.L;
  Cont(time)             = Con.L;
  GovCt(time)            = GovC.L;
  Invt(time)             = Inv.L;
  Inv1t(time)            = Inv1.L;
  Txt(time)              = Tx.L;
  YLt(time)              = YL.L;
  YKt(time)              = YK.L;
  Con1t(time)            = Con1.L;
  Rset(time,s1,ed)       = Rse.L(s1,ed);
  Mst(time,s1)           = Ms.L(s1);
  Mact(time)             = Mac.L;
  Robt(time)             = Rob.L;
  Ktot                   = KN.L;
);

Display   Population, Births, Populationtotal, EducRatio, EducPop, Cont, Con1t, Mact, Robt, INVt, INV1t, GovCt, Cont, GDPt, GovCt, Txt,
          GNPt,GDPt, Kt, Rset, Mst, Kt, Kttest,KN.L, KNtest.L,Con.L, Con1.L,
          INV.L, INV1.L, Sav.L, Wedt;
