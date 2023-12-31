**Keyvan Tahmasbi (ktahmasbi77@gmail.com)**
# فصل 2: رگرسیون و توزیع نرمال

## شبیه سازی  و برآورد پارامترهای مدل

**مرحله اول**: تولید نمونه تصادفی بر اساس مدل پیشنهادی

ابتدا متغیرهای تصادفی و پارامترها را در مدل پیشنهادی تعیین می کنیم.

متغیرهای تصادفی از توزیع آماری مشخص (اینکه توزیع چه باشد را بر اساس دیتاهای واقعی می توانیم تشخیص دهیم.) تولید می کنیم و سپس به پارامترهای مدل مقدار واقعی می دهیم.

**مرحله دوم:** مدل پیشنهادی را با دیتاهای تولید شده برازش می دهیم. سپس پارامترهای مدل را برآورد می کنیم.

**مرحله سوم:** مرحله اول و دوم را m بار تکرار میکنیم.

**مرحله چهارم:** با هر بار تکرار، برآوردهای متفاوتی را برای پارامتر های مدل بدست می آوریم. 
 $(\hat{_{0i}\beta})$. سپس از آنها میانگین میگیریم:

 $$\bar{\hat{\beta_{0i}}} = \frac{1}{m} \sum_{i=1}^m \hat{\beta_{0i}}$$

 لازم است که خطای استاندارد را نیز بدست آوریم تا بتوانیم مقایسه خوبی بین برآورد پارامترها با مقدار واقعی آنها را داشته باشیم.

 $$S.E_\theta = \sqrt{\widehat{var{\hat{(\theta)}}}}$$

بطوری که

 $$var(\hat{\theta}) = \frac{1}{I_x(\theta)} $$

 $${I_x(\theta)} =E(\frac{d}{d\theta} lnf_\theta(x))^2 = -E(\frac{d^2}{d\theta^2}lnf_\theta(x)) = قطر ماتریس 
  $$

 #





### شبیه سازی توزیع نرمال یک متغیره
 برای مثال مدل زیر را در نظر بگیرید:


$$ y = \beta_0 + \beta_1x_1 + \beta_2x_2 + \varepsilon $$

بطوری که:

$$x_{1i} \sim N(0, 1)$$
$$x_{2i} \sim Ber(0.5)$$
$$ \varepsilon_i \sim N(0,0.5)$$
$$\beta_0 = 0 , \beta_1=1, \beta_2=-1, \sigma^2=0.5$$

 داریم:

$$y_i \sim N(\beta_0 + \beta_1 x_{1i}+\beta_2x_{2i}, \sigma^2)$$


متغیرهای تصادفی از توزیع های آماری مشخص تولید می کنیم (مثلا برای $_1x$ توزیع نرمال و برای $_2x$ توزیع برنولی در نظر میگیریم.) سپس به پارامتر های مدل ($_0\beta$ و $_1\beta$ و $_2\beta$ و $^2\sigma$) مقدار واقعی می دهیم.


برای $\varepsilon$ توزیع نرمال درنظر میگیریم ومقادیر را برای آن تولید می کنیم.

بر اساس آن مقادیر متغیرهای مستقل ($_1x$ و $_2x$) را تولید می کنیم.

سپس مدل را میسازیم.


```R
n = 50
x1 = rnorm(n,0, 1)
x2 = rbinom(n,1,0.5)
eps = rnorm(n,0,0.5)
beta0 = 0
beta1 = 1
beta2 = 1
y = beta0 + beta1*x1 + beta2*x2 + eps
cbind(y, x1, x2)
```
#
### توزیع نرمال چند متغیره

$$\textbf{X}=(X_1,\dots,X_p)^T$$

$$\textbf{X} \sim N_p(\underline{\mu} , \underline{\Sigma}) $$

$$\mu = (\mu_1, ... , \mu_p)^T$$

```math
$$
\underline{\Sigma} =\Sigma_{i,j} =D(\textbf{X})= E[(X_i - E(X_i))(X_j-E(X_j))]=Cov[X_i, X_j] = \begin{pmatrix} \sigma_1^2 & \sigma_{12} & \dots & \sigma_{1p} \\ 
\sigma_{12} & \sigma_2^2 & \dots & \sigma_{2p} \\ \vdots & \vdots & \ddots & \vdots \\ \sigma_{p1} & \sigma_{p2} & \dots & \sigma_p^2 \end{pmatrix}
$$
```

$$f(\textbf{X})=f(x_1,...,x_p) = (2\pi)^{-\frac{n}{2}} \Sigma^{-{\frac{1}{2}}} exp[-\frac{1}{2}(x-\mu)^T \Sigma^{-1}(x-\mu)] $$

همیشه $\Sigma$ مثبت است.

اگر $0 = |\Sigma|$، توزیع $\textbf{X} $  نرمال تکین (sigular) است.

زمانی که $\sigma^2 \textbf{I}=|\Sigma|$ تابع مشخصه $N_p(\mu , \Sigma) \sim \textbf{X}$ به تابع مشخصه های توزیع نرمال یک متغیره مولفه های $\textbf{X}$ تبدیل می شود. در نتیجه در این رابطه مولفه های $\textbf{X}$ متغیرهای نرمال مستقل از هم هستند. پس تابع چگالی توام نرمال چند متغیره می شود ضرب چگالی های کناری تک تک آنها. یعنی


$$
f(\textbf{X}) = \prod_{i=1}^p f(x_1,...,x_p) = \prod_{i=1}^p (2 \pi \sigma^2)^{-\frac{1}{2}} exp[-\frac{{(x_i - \mu_i)^2}}{2\sigma^2}] = (2 \pi \sigma^2)^{-\frac{p}{2}} exp[-\frac{1}{2 \sigma^2}(\textbf{X}-\mu)^T \Sigma^{-1}(\textbf{X}-\mu)]
$$


# 
### شبیه سازی توزیع نرمال دو متغیره

میخواهیم تابع چگالی توزیع نرمال دو متغیره را با توجه به فرمول بالا بدست آوریم. 

```math
$$
\textbf{X} = \begin{pmatrix} x \\ y \end{pmatrix}
,
\mu = \begin{pmatrix} \mu_{x} \\ \mu_{y} \end{pmatrix}
,
\Sigma =  \begin{pmatrix} \sigma^2_{x} & \sigma_{x y} \\ \sigma_{x y} & \sigma^2_{y}  \end{pmatrix} = \begin{pmatrix} \sigma^2_{x} & \rho \sigma_{x} \sigma_{y} \\ \rho \sigma_{x} \sigma_{y} & \sigma^2_{y}  \end{pmatrix}
$$
```
همبستگی بین x, y:

$\rho = \frac{\sigma_{xy}}{\sigma_{x} \sigma_{y}}$

پارامترهای توزیع کناری:

$E(x) = \mu_{x}$ , $E(y)=\mu_{y}$ , $var(x)=\sigma_{x}^2$ , $var(y) = \sigma_{y}^2$



داریم:

$$
|\Sigma| =\sigma_x^2 \sigma_y^2 - \sigma^2_{xy} =\sigma_x^2\sigma_y^2(1- \frac{\sigma_{xy}^2}{\sigma_x^2 \sigma_y^2}) =  \sigma_x^2\sigma_y^2(1-\rho^2)
$$

```math
$$
\Sigma^{-1} = \frac{1}{|\Sigma|} \begin{pmatrix} \sigma^2_y & -\sigma_{xy} \\ -\sigma_{xy} & \sigma^2_x\end{pmatrix} = \frac{1}{1- \rho^2} \begin{pmatrix} \frac{1}{\sigma^2_x} & -\frac{\rho}{\sigma_x \sigma_y} \\ -\frac{\rho}{\sigma_x \sigma_y} & \frac{1}{\sigma^2_y}\end{pmatrix} 
$$
```
در نتیجه:

$$
(\textbf{y}-\mu)^T |\Sigma|^{-1}(\textbf{y}-\mu) = \frac{1}{1-\rho^2}[(\frac{x-\mu_x}{\sigma_x})^2+(\frac{y-\mu_y}{\sigma_y})^2 -2\rho (\frac{x-\mu_x}{\sigma_x})(\frac{y-\mu_y}{\sigma_y})] 
$$

بنابراین تابع چگالی نرمال دو متغیره برابر است با:

$$
f(x,y) = \frac{1}{2 \pi \sigma_x \sigma_y \sqrt{1-\rho^2}}exp[-\frac{1}{2(1-\rho^2)} \{(\frac{x-\mu_x}{\sigma_x})^2\} + \{(\frac{y-\mu_y}{\sigma_y})^2 -2 \rho (\frac{x-\mu_x}{\sigma_x})(\frac{y-\mu_y}{\sigma_y}) \} ]
$$

استقلال، $0 = \rho$ را نتیجه می دهد ولی عکس آن صادق نیست. یعنی اگر $0 = \rho$ الزاما متغیرها از هم مستقل نیستند بجز در توزیع نرمال دو متغیره.

حال می خواهیم توزیع توام نرمال دو متغیره را تولید کنیم:

```R
library(MASS)
n = 50

# Simulate from a Multivariate Normal Distribution
mu = c(0,0)
S = matrix(c(10,3,3,2),2,2)
X = mvrnorm(n, mu, S)
y = mvrnorm(n, 0, 1)

# log likelihood
p = c(-1,1)
L = rep(0,n)
f = function(p){
    for(i in 1:n){
        L[i] = log(dnorm(y[i], p[1], p[2]))
    }
    Lfinal = sum(L)
    return(-Lfinal)
}
f(p)
```
ابتدا از توزیع بردار متغیر مستقل داده تولید می کنیم. به همین دلیل از شبیه سازی استفاده میکنیم. برای برآورد پارامترهای مدل نیاز است که تابع درستنمایی را بدست آوریم و از آن لگاریتم بگیریم. سپس برای اینکه MLE یا برآورد بزرگترین درستنمایی را بدست آوریم logL- را محاسبه میکنیم و در آخر برآورد پارامترها را که جلوتر به آن میپردازیم را بدست می آوریم. (در حالت محاسبه دستی پس از اینکه تابع درستمنایی را بدست آوردیم، از آن نسبت به پارامترهای مدل مشتق میگرفتیم و برابر با صفر قرار میدادیم. مشتق گیری حکم بدست آوردن بیشترین مقدار بود. سپس برآورد پارامترها بدست می آمد.)

#### مثال: توزیع نرمال سه متغیره؟؟؟



# 
## توزیع نرمال شرطی

داریم:

$$\textbf{Y} \sim N(\mu, \Sigma)$$

```math
$$
\textbf{Y} = \begin{pmatrix} y_1 \\ y_2 \end{pmatrix} , 
\mu = \begin{pmatrix} \mu_1 \\ \mu_2 \end{pmatrix} ,
\Sigma= \begin{pmatrix} \Sigma_{11} & \Sigma_{12} \\ \Sigma_{21} & \Sigma_{22} \end{pmatrix}
$$
```
اگر $_{11}\Sigma$ ناتکین باشد داریم:

$$
E[y_2|y_1] = \mu_2+ \Sigma_{21} \Sigma_{11}^{-1}(y_1 - \mu_1)
$$

$$
D(y_2|y_1) = Var(y_2|y_1) =\Sigma_{22} - \Sigma_{21} \Sigma_{11}^{-1}\Sigma_{12}
$$

پس توزیع شرطی می شود:

$$
y_2|y_1 \sim N(\mu_2+ \Sigma_{21} \Sigma_{11}^{-1}(y_1 - \mu_1), \Sigma_{22} - \Sigma_{21} \Sigma_{11}^{-1}\Sigma_{12})
$$

### مثال: توزیع نرمال شرطی دو متغیره

برای توزیع نرمال شرطی دو متغیره داریم:

$$
E(y|x) = \mu_y + \frac{\sigma_{yx}}{\sigma_x^2}(x-\mu_x) = \mu_y + \rho \frac{\sigma_y}{\sigma_x}(x-\mu_x)
$$

$$
Var(y|x) = \sigma_y^2 - \sigma_{yx} \frac{1}{\sigma_x^2} \sigma_{xy} = \sigma_y^2 - \frac{\sigma_{xy}^2}{\sigma_x^2} =\sigma_y^2(1-\rho^2)
$$

داریم:

$$
Var(y|x) < Var(y) \Rightarrow (1-\rho^2)\sigma ^2_y < \sigma_y^2
$$

حالت تساوی زمانی برقرار است که $0=\rho$. در این حالت میانگین شرطی نیز با میانگین غیر شرطی برابر است.

### مثال

فرض کنید توزیع زیر را داریم.

```math
$$
Y = \begin{pmatrix} y_1 \\ y_2 \end{pmatrix} \sim N_2(\begin{pmatrix} 0 \\ 0 \end{pmatrix}, \begin{pmatrix} 1 & 0.5 \\ 0.5 &1 \end{pmatrix})
$$
```

```R
# Conditional bivariate normal distribution page 33 - example 2.3
library(MASS)

n = 100
mu = c(0,0)
S = matrix(c(10,0.5,0.5,1),2,2)
y = mvrnorm(n , mu, S) #library(MASS)
y2 = y[,2]
mu1 = mu2 = 0
S1 = S2 = 1
rho = 0.5

mu1gmu2 = mu1 + rho*S1/S2*(y2 - mu2)
S1gS2 = (S1)^2*(1-rho^2)
y1gy2 = rnorm(n, mu1gmu2, S1gS2)
y1gy2

hist(y1gy2)
heatmap(y)
```
### مثال:
فرض کنید توزیع شرطی **متغیر تصادفی y** به شرط **بردار تصادفی X** تولید شده از توزیع توام آنها، نرمال باشد.

```math
$$
Y = \begin{pmatrix} \underline{X} \\ y \end{pmatrix}
$$
```
میخواهیم توزیع $y|X$ را تولید کنیم.

داریم:

$$
y|X \sim MN(E(y|X), Var(y|X))
$$

بطوری که:

```math
$$
E(y|X) = \mu_y + \Sigma_{yX} \Sigma_{XX}^{-1} (X-\mu_X) = \mu_y + \sigma'_{Xy} \Sigma_{XX}^{-1}(X-\mu_X)
$$


$$
Var(y|X) = \sigma_y^2 - \sigma'_{Xy} \Sigma_{XX}^{-1} \sigma_{Xy}
$$
```

#
### مثال
فرض کنید ماتریس پراکندگی متغیرها $_1y$ , $_2y$, $_3y$ به صورت زیر است:

```math
$$
\begin{pmatrix} 1 & 0.5 & 0.5 \\ 0.5 & 1 & 0.25 \\ 0.5 & 0.25 & 1 \end{pmatrix}
$$
```

توزیع $y1,y2|y3$ را شبیه سازی کنید.

داریم:

```R
library(MASS)

n = 1000
S = matrix(c(1,0.5,0.5,0.5,1,0.25,0.5,0.25,1),3,3)
mu = c(0,0,0)
Y = mvrnorm(n, mu, S)

S3 = 1
S12 = matrix(c(1,0.5,0.5,1),2,2)
S12S3 = matrix(c(0.5,0.25),2,1)
S1S2gS3 = S12 - S12S3 %*% S3 %*% t(S12S3)

mu12 = c(0,0)
y3 = Y[,3]
mu3 = 0
mu1mu2gmu3 = y1y2gy3 = array(0,c(2,1,n))
for(i in 1:n){
    mu1mu2gmu3[,,i] = mu12 + S12S3*S3*(y3[i] - mu3)
    y1y2gy3[,,i] = mvrnorm(1, c(mu1mu2gmu3[1,1,i],mu1mu2gmu3[2,1,i]), S1S2gS3)

}
y1y2gy3

```
# 

# فصل 3

## مدل سازی، برازش مدل، برآورد پارامترها

میخواهیم مدلی را بسازیم. دو روش وجود دارد. 

روش اول:

```R
library(lmreg)
library(MASS)

data(stars1) # library lmreg
lmstars = lm(Velocity~Distance, data=stars1)
lmstars
```

در کد بالا lmreg برآورد رگرسیونی به صورت ناپارامتری است و lm به صورت پارامتری است.

حال میخواهیم مدل بالا را مستقیم بدست آوریم


روش دوم:

ابتدا نیاز است که داده تولید کنیم. سپس برآورد پارامترها را بدست آوریم (LM). برای بدست آوردن بذآورد پارامتر ها به تابع درستنمایی نیاز داریم.

بعد از اینکه برآورد پارامترها را بدست آوردیم لازم است که خطای استاندارد را نیز بدست آوریم تا بتوانیم مقایسه خوبی بین برآورد پارامترها با مقدار واقعی آنها را داشته باشیم. برای بدست آوردن خطای استاندارد لازم است ماتریس اطلاع فیشر را محاسبه کنیم.

```R
library(MASS)
library(nlme)

n = 10000
eps = rnorm(n, 0, 1)
X = rnorm(n, 0, 1)
B1 = 0.5
B0 = 0
Y = B0 + B1*X + eps

data = cbind(Y,X)

p = c(B0, B1, 1)
L = rep(0,n)
f = function(p){
    for(i in 1:n){
    L[i] = log(dnorm(Y[i], p[1]+p[2]*X[i], p[3]))
    }
    Lfinal = sum(L)
    return(-Lfinal)
}
f(p)

LM = nlminb(p, f, lower=c(-Inf,-Inf,0), upper=c(Inf, Inf, Inf))
LM
MLE = LM$par

h = fdHess(MLE, f) # library nlme
ih = solve(h$Hessian)
se = sqrt(diag(ih))
cbind(p, MLE, se)
```

با n های متفاوت نیز کد بالا را امتحان میکنیم. هر چه n بیشتر شود برآورد پارامتر به مقدار واقعی پارامتر نزدیک تر میشود و مقدار se کاهش میابد.


## مقادیر برازش داده شده و باقی مانده ها

مقادیر برازش داده شده به y را به این صورت تعریف کردیم:

$$
\hat{y} = X(X'X)^{-1}X'y = Hy
$$

اگر $X'X$ تکین باشد، معکوس بالا می شود معکوس تعمیم یافته. 

می خواهیم مقادیر برازش داده شده را بیابیم:

به دو روش می توان این مقادیر را بدست اورد:

روش اول:

```R
library(lmreg)

data(lifelength) # library lmreg
head(lifelength)
attach(lifelength)

X = cbind(1, binaries(Category))
H = X %*% ginv(t(X) %*% X) %*% t(X)
y = Lifelength
yhat = H %*% y

```

روش دوم:

```R
library(lmreg)

data(lifelength) # library lmreg
head(lifelength)
lm = lm(Lifelength~factor(Category), data = lifelength)
fitted = lm$fit
```

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
پیرسون: پیوسته - پیوسته
بی سریال: پیوسته-کیفی
اسپیرمین: ترتیبی-ترتیبی و پیئسته-ترتیبی

بررسی هم خطی(VIF: variance inflation function)


#
ابتدا ارتباط متغیرها را با یکدیگر بررسی میکنیم. نمیتوانیم به طور قطعی تصمیم بگیریم چون داریم از EDA استفاده میکنیم و آمار استنباطی نیست.

برای بررسی نموداری ارتباط از نمودار نقطه ای(پیوسته-پوسته) و باکس پلات (پیوسته-کیفی) استفاده میکنیم. روش غیر نموداری بررسی خود واریانس هاست اگر واریانس ها دور و بر هم نباشند ناهمگونی واریانس ها داری و این خوب نیست. از نمودار باقی مانده ها نیز میتوان همگونی واریانس ها را تشخیص داد برای داده های پیوسته.

نمودار باکس پلات همگونی واریانس ها را نیز نشان میدهد. (باید همگونی واریانس داشته باشیم اگر ندشته باشیم روش های برآوردیابی مثل BLUE و ... کارایی ندارد.)

اگر همه متغیرها پیوسته باشند از نمودار پراکندگی استفاده میکنیم.

میتوان هم خطی را با رکش نموداری نیز بررسی کرد. 

از VIF میتوان هم خطی را نیز تشخیص داد.
اگر VIF کوچکتر از 2 باشد هم خطی نداریم.بین 2 تا 5 هم خطی خفیف داریم. از 10 به بالا هم خطی شدید داریم.

برای بررسی نرمالیتی به روش نموداری از نمودار هیستوگرام یا qq-plot یا pp-plot استفاده میکنیم.

روش غیر نموداری بررسی نرمالیتی را میتوان با توجه به  میانگین و مد و میانه فهمید. اگر نرمال باشد این اعداد نزدیک به هم هستند.

اگر -log(L) کم شود یعنی مدل خوب است.

معیار AIC=-2log(L) - 2p هر چه کمتر باشد مدل بهتر است.
#
# فصل 4
 در این فصل آمار استنباطی را انجام می دهیم. سپس پارامترهای مدل را برآورد میکنیم. برای برآورد پارامترهای مدل نیاز است که تابع درستنمایی را بدست آوریم.

 












