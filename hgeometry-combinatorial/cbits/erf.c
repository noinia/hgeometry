#include <math.h>

double erf_log(double number, double error, double result)
{
  long double precise = logl((long double)number + (long double)error);
  return (double) (precise - (long double)result);
}

double erf_exp(double number, double error, double result)
{
  long double precise = expl((long double)number + (long double)error);
  return (double) (precise - (long double)result);
}

double erf_pow(double n1, double e1, double n2, double e2, double result)
{
  long double precise = powl((long double)n1 + (long double)e1, (long double)n2 + (long double)e2);
  return (double) (precise - (long double)result);
}

double erf_sin(double number, double error, double result)
{
  long double precise = sinl((long double)number + (long double)error);
  return (double) (precise - (long double)result);
}

double erf_cos(double number, double error, double result)
{
  long double precise = cosl((long double)number + (long double)error);
  return (double) (precise - (long double)result);
}

double erf_tan(double number, double error, double result)
{
  long double precise = tanl((long double)number + (long double)error);
  return (double) (precise - (long double)result);
}

double erf_asin(double number, double error, double result)
{
  long double precise = asinl((long double)number + (long double)error);
  return (double) (precise - (long double)result);
}

double erf_acos(double number, double error, double result)
{
  long double precise = acosl((long double)number + (long double)error);
  return (double) (precise - (long double)result);
}

double erf_atan(double number, double error, double result)
{
  long double precise = atanl((long double)number + (long double)error);
  return (double) (precise - (long double)result);
}

double erf_sinh(double number, double error, double result)
{
  long double precise = sinhl((long double)number + (long double)error);
  return (double) (precise - (long double)result);
}

double erf_cosh(double number, double error, double result)
{
  long double precise = coshl((long double)number + (long double)error);
  return (double) (precise - (long double)result);
}

double erf_tanh(double number, double error, double result)
{
  long double precise = tanhl((long double)number + (long double)error);
  return (double) (precise - (long double)result);
}

double erf_asinh(double number, double error, double result)
{
  long double precise = asinhl((long double)number + (long double)error);
  return (double) (precise - (long double)result);
}

double erf_acosh(double number, double error, double result)
{
  long double precise = acoshl((long double)number + (long double)error);
  return (double) (precise - (long double)result);
}

double erf_atanh(double number, double error, double result)
{
  long double precise = atanhl((long double)number + (long double)error);
  return (double) (precise - (long double)result);
}

double erf_atan2(double n1, double e1, double n2, double e2, double result)
{
  long double precise = atan2l((long double)n1 + (long double)e1, (long double)n2 + (long double)e2);
  return (double) (precise - (long double)result);
}
