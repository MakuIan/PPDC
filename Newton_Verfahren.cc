#include <iostream>
#include <cmath>

//Typ-Definition: Zeiger auf f: double -> double
typedef double (*ftype)(double);

//Numerisches Differenzieren (funktional)
double ableitung(const ftype f, double h, double xk){
    return (f(xk+h) - f(xk)) / h;
}

//Newton-Verfahren (prozedural, mit call-by-reference)
void newton(const ftype myfunction, double& xk){
    while(true){    //Endlosschleife            
        double fk = myfunction(xk);                         //Zuweisung
        if (fabs(fk) < 1e-6) break;                         //Sprungbedingung
        xk = xk - fk / ableitung(myfunction, 1e-5, xk);     //Update via Zuweisung
    }
}

//Newton-Verfahren im Hauptprogramm
int main(){
    double xk = 1.0;
    newton(sin, xk);
    printf("Nullstelle x=%f, f(x)=%f\n", xk, sin(xk));
    return 0;
}