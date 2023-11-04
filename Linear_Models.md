**Keyvan Tahmasbi (ktahmasbi77@gmail.com)**
# فصل 2: رگرسیون و توزیع نرمال
### توزیع نرمال دو متغیره
$\textbf{y} = \begin{pmatrix} x \\ y \end{pmatrix}$ 

$\mu = \begin{pmatrix} \mu_x \\ \mu_y \end{pmatrix}$

$\Sigma =  \begin{pmatrix} \sigma^2_x & \sigma^2_{xy} \\ \sigma^2_{xy} & \sigma^2_y \end{pmatrix}$

پارامترهای توزیع کناری:

$E(x) = \mu_x$ , $E(y)=\mu_y$ , $var(x)=\sigma_x^2$ , $var(y) = \sigma_y^2$

همبستگی بین x, y:

$\rho = \frac{\sigma_{xy}}{\sigma_x \sigma_y}$


# فصل 3

## شناساپذیری و برآوردپذیری
می خواهیم شناساپذیری را چک کنیم. 

پارامتری که شناساپذیر باشد، برآوردپذیر است اما عکس آن ممکن است صادق نباشد. 

مثال:

$$f_{\theta_1,\theta_2}(x) = \frac{\theta_1 }{\theta_2} e^{\frac{\theta_1 }{\theta_2}x} $$

نکته: در مواردی که پارامتر ها تقسیم، تفریق  شده اند معمولا شناساپذیری نداریم.

ما نمیتوانیم براوردهای جداگانه ای برای $_1\theta$ و $_2\theta$ بدست آوریم. برای همین طبق قضیه زهنا می توانیم برآوردی برای $\frac{_1\theta}{_2\theta}$ بدست آوریم. 

شناساپذیری:

$$\theta_1 \neq \theta_2 \Leftrightarrow f_{\theta_1}(x) \neq f_{\theta_2}(x) $$
 
```R
# identifibility
theta1 = 1
theta2 = 1
lambda = theta1/theta2
X = rexp(100, lambda) 
hist(X)

# MLE
p = c(1,1)
L = rep(0,100)
f = function(p){
    for(i in 1:100){
        L[i] = dexp(X[i], p[1]/p[2])
    }
    Lfinal = sum(log(L))
    ;-Lfinal
}
f(p)
p = c(2,2)

```
وقتی p=c(1,1) برای برآورد تتا1 داریم 94.97792 و وقتی p = c(2,2 ) برای برآورد تتا2 نیز داریم 94.97792. پس شناساپذیر نیست. یعنی نمیتوانیم برآوردی برای تتا1 و تتا2 به طور جداگانه بدست آوریم. اگرچه که نرم افزار این کار را انجام می دهد ولی کاری غلط است. 

```R
M = nlminb(p, f ,lower=c(0,0, upper=c(Inf,Inf)))
M
library(nlme)
h = fdHess(M$par, f)
ih = solve(h$Hessian)
se = diag(sqrt(ih))
MLE = M$par
Results = cbind(p, MLE, se) 
> Results
     p      MLE       se
[1,] 2 2.076198 52.73225
[2,] 2 1.971930 50.08399
```
انحراف استاندارد (se) اگر خیلی بزرگ یا صفر باشد بیانگر شناساناپذیری پارامترها می باشد. 

برای اینکه بتوانیم پارامتر ها را شناساپذیر کنیم نیاز است که یکس از آنها را ثابت در نظر بگیریم 

```R
f = function(p){
    p[2] = 2
    for(i in 1:100){
        L[i] = dexp(X[i], p[1]/p[2])
    }
    Lfinal = sum(log(L))
    ;-Lfinal
}
f(p)
p = 2
M = nlminb(p, f ,lower=0 , upper=Inf)
M
library(nlme)
h = fdHess(M$par, f)
ih = solve(h$Hessian)
se = diag(sqrt(ih))
MLE = M$par
Results = cbind(p, MLE, se)
> Results
     p      MLE        se
[1,] 2 2.105752 0.2105755
```
در هر برآوردیابی ابتدا واریانس را بدست می اوریم و بعد مانده ها را تحلیل میکنیم.
مانده ها به ما میگوید داده ها ناهنجاری دارد یا خیر. ص74

حدود باقی مانده ها اگر خارج از -3 و +3 باشد یعنی مشکلی در جواب ها وجود دارد.

وقتی باقی مانده ها تغییرات زیاد داشته باشد نشان می دهد جواب ها از روایی و پایایی برخوردار نیستند.

الگوریتم فلمینگ هالت به ما می گوید آیا جواب ها با هم همخوانی دارد یا خیر.


## فرضیات مدل
بررسی نرمالیتی(نمودار-ضریب چولگی-چارک ها-میانگین و میانه و مد دور و بر هم باشند)

همگونی واریانس

بررسی همبستگی(پیرسون-اسپیرمن-بی سریال)

بررسی هم خطی(VIF: variance inflation function)