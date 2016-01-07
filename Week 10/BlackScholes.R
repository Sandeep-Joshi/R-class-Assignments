call.BS<-function(S, K, r, Maturity,sigma)
{
    d1<-(log(S/K)+(r+(sigma^2)/2)*Maturity)/(sigma*sqrt(Maturity));
    d2<-d1-sigma*sqrt(Maturity);
    S*pnorm(d1)-K*exp(-r*Maturity)*pnorm(d2)
}


put.BS<-function(S, K, r, Maturity,sigma)
{
    d1<-(log(S/K)+(r+(sigma^2)/2)*Maturity)/(sigma*sqrt(Maturity));
    d2<-d1-sigma*sqrt(Maturity);
    K*exp(-r*Maturity)*pnorm(-d2)-S*pnorm(-d1)
}