Transmission in the Meltzer *et al.* model is based on

 - the number of infectious individuals,
 - their distribution to the different treatment categories, and
 - the number of remaining susceptible individuals (refered to as the *population governor* in original publication)
 
In the default model, the infectious individuals are divided into three control categories:
 
 - hospitalized, meaning individuals that are in Ebola Treatment Units (ETUs) for their infectious period
 - isolating home care, meaning individuals that are cared for in the community in a way that minimizes Ebola transmission, and finally
 - non-isolating home care, meaning care in the community in a way that does not control Ebola transmission
  
These control divisions are also intended to include differences in burial practices (and thus danger of post-mortem transmission) between these three settings.

We call these control categories $h$, $c$, and $n$ (for *hospital*, *community*, and *none*), making the per capita infection rate:

$$
{\vec{\beta} \cdot \vec{I} \over N} = {\sum_{i\in (h,c,n)} \beta_i I_i \over N}
$$

and thus the incidence of new infections on day $t$ is

$$
E_t^+ = {\vec{\beta} \cdot \vec{I}_{t-1} \over N_{t-1}} S_{t-1}
$$

We keep track of the number of susceptibles by deducting the cumulative incidence from the initial population:

$$
S_t = N_0 - \sum_{t'=0}^t E_{t'}^+
$$

In the default model, $N_t = N_0$ for all times.
