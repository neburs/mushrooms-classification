
# Mushrooms Classification

I try to create a Model classification to try to predict if a Mushroom with a specific characteristics is edible or not. To do this I decide to use a tree classification using the C.50 algorithm.

Next, I specify the description of each attribute and their values available.

Attribute Information: (classes: edible=e, poisonous=p)
* **cap-shape**: bell=b,conical=c,convex=x,flat=f, knobbed=k,sunken=s
* **cap-surface**: fibrous=f,grooves=g,scaly=y,smooth=s
* **cap-color**: brown=n,buff=b,cinnamon=c,gray=g,green=r,pink=p,purple=u,red=e,white=w,yellow=y
* **bruises***: bruises=t,no=f
* **odor**: almond=a,anise=l,creosote=c,fishy=y,foul=f,musty=m,none=n,pungent=p,spicy=s
* **gill-attachment**: attached=a,descending=d,free=f,notched=n
* **gill-spacing**: close=c,crowded=w,distant=d
* **gill-size**: broad=b,narrow=n
* **gill-color**: black=k,brown=n,buff=b,chocolate=h,gray=g, green=r,orange=o,pink=p,purple=u,red=e,white=w,yellow=y
* **stalk-shape**: enlarging=e,tapering=t
* **stalk-root**: bulbous=b,club=c,cup=u,equal=e,rhizomorphs=z,rooted=r,missing=?
* **stalk-surface-above-ring**: fibrous=f,scaly=y,silky=k,smooth=s
* **stalk-surface-below-ring**: fibrous=f,scaly=y,silky=k,smooth=s
* **stalk-color-above-ring**: brown=n,buff=b,cinnamon=c,gray=g,orange=o,pink=p,red=e,white=w,yellow=y
* **stalk-color-below-ring**: brown=n,buff=b,cinnamon=c,gray=g,orange=o,pink=p,red=e,white=w,yellow=y
* **veil-type**: partial=p,universal=u
* **veil-color**: brown=n,orange=o,white=w,yellow=y
* **ring-number**: none=n,one=o,two=t
* **ring-type**: cobwebby=c,evanescent=e,flaring=f,large=l,none=n,pendant=p,sheathing=s,zone=z
* **spore-print-color**: black=k,brown=n,buff=b,chocolate=h,green=r,orange=o,purple=u,white=w,yellow=y
* **population**: abundant=a,clustered=c,numerous=n,scattered=s,several=v,solitary=y
* **habitat**: grasses=g,leaves=l,meadows=m,paths=p,urban=u,waste=w,woods=d

The attribute to clasificate is *classes* that it indicate if the mushroom is edible or not.

Next, I load the dataset and check if the attributes have the values correctly informed or if they have any strange value.


```R
install.packages("xlsx", dep = T)
library(xlsx)

mushrooms <- read.csv("mushrooms.csv", header = TRUE, sep = ',')

summary(mushrooms)
```
    class    cap.shape cap.surface   cap.color    bruises       odor      gill.attachment gill.spacing gill.size   gill.color  
    e:4208   b: 452    f:2320      n      :2284   f:4748   n      :3528   a: 210          c:6812       b:5612    b      :1728  
    p:3916   c:   4    g:   4      g      :1840   t:3376   f      :2160   f:7914          w:1312       n:2512    p      :1492  
             f:3152    s:2556      e      :1500            s      : 576                                          w      :1202  
             k: 828    y:3244      y      :1072            y      : 576                                          n      :1048  
             s:  32                w      :1040            a      : 400                                          g      : 752  
             x:3656                b      : 168            l      : 400                                          h      : 732  
                                   (Other): 220            (Other): 484                                          (Other):1170  
    stalk.shape stalk.root stalk.surface.above.ring stalk.surface.below.ring stalk.color.above.ring stalk.color.below.ring
    e:3516      ?:2480     f: 552                   f: 600                   w      :4464           w      :4384          
    t:4608      b:3776     k:2372                   k:2304                   p      :1872           p      :1872          
                c: 556     s:5176                   s:4936                   g      : 576           g      : 576          
                e:1120     y:  24                   y: 284                   n      : 448           n      : 512          
                r: 192                                                       b      : 432           b      : 432          
                                                                             o      : 192           o      : 192          
                                                                             (Other): 140           (Other): 156          
    veil.type veil.color ring.number ring.type spore.print.color population habitat
    p:8124    n:  96     n:  36      e:2776    w      :2388      a: 384     d:3148  
              o:  96     o:7488      f:  48    n      :1968      c: 340     g:2148  
              w:7924     t: 600      l:1296    k      :1872      n: 400     l: 832  
              y:   8                 n:  36    h      :1632      s:1248     m: 292  
                                     p:3968    r      :  72      v:4040     p:1144  
                                               b      :  48      y:1712     u: 368  
                                               (Other): 144                 w: 192


I can see that **all the attributes** of this dataset **are qualitative** and a simple view, I can not see nothing strange. (I can **not see the necessity to discretize or add new attributes**). I can **not detect empty values**, but if you look the **attribute veil.type**, it **only have** observations with **an unique value** that the two values available. This makes us suspect that the dataset may not be entirely representative of reality and may need more observations to be able to represent reality. At the moment **I eliminate this attribute** from the dataset since it does not make sense to add it to the classifier. Maybe later with more observations if it would be sensible to add it for a better classification.

```R
mushrooms$veil.type <- NULL
```

Next, I check the 16 first observations to check that this dataset is not ordered by the attribute to classify.

```R
head( mushrooms[1], 16 )
```
    class
    1      p
    2      e
    3      e
    4      p
    5      e
    6      e
    7      e
    8      e
    9      p
    10     e
    11     e
    12     e
    13     e
    14     p
    15     e
    16     e

I check that the dataset is not ordered by the attribute to classify. Next, I proceed to create the classification model.

```R
# Create the dataset to test and training
X <- mushrooms[,2:22]
y <- mushrooms[,1]

# Calculate split 1/3 to test. This number is where the dataset must split the values
split <- length(mushrooms$class) - round(length(mushrooms$class)/3)

trainInputs <- X[1:split,]
trainOutput <- y[1:split]
testInputs <- X[ (split + 1):length(mushrooms$class),]
testOutput <- y[(split + 1):length(mushrooms$class)]

install.packages("C50", dep = T)
library(C50)

# I create the model
model <- C50::C5.0( trainInputs, trainOutput )
summary( model )
```
    Call:
    C5.0.default(x = trainInputs, y = trainOutput)


    C5.0 [Release 2.07 GPL Edition]  	Sun Nov  5 23:04:10 2017
    -------------------------------

    Class specified by attribute `outcome'

    Read 5416 cases (22 attributes) from undefined.data

    Decision tree:

    odor = m: e (0)
    odor in {c,f,p,s,y}: p (1835)
    odor in {a,l,n}:
    :...stalk.surface.above.ring = y: e (0)
        stalk.surface.above.ring = k: p (24)
        stalk.surface.above.ring in {f,s}:
        :...spore.print.color in {b,o,y}: e (0)
            spore.print.color = r: p (24)
            spore.print.color in {h,k,n,u,w}:
            :...cap.surface in {f,s,y}: e (3530/3)
                cap.surface = g: p (3)


    Evaluation on training data (5416 cases):

    	    Decision Tree   
    	  ----------------  
    	  Size      Errors  

    	     5    3( 0.1%)   <<


    	   (a)   (b)    <-classified as
    	  ----  ----
    	  3527          (a): class e
    	     3  1886    (b): class p


    	Attribute usage:

    	100.00%	odor
    	 66.12%	stalk.surface.above.ring
    	 65.68%	spore.print.color
    	 65.23%	cap.surface


    Time: 0.0 secs

![GitHub Logo](/images/tree.png)

I can see in the model, the next classification rules:

* If (Odor = m *"musty"*) then is edible.
* If (Odor is any of [c *"creosote"*, f *"foul"*, p *"pungent"*, s *"spicy"*, y *"fishy"*]) then is poisonous.
* If (Odor is any of [a *"almond"*, l *"anise"*, n *"none"*]) & (stalk.surface.above.ring = y *"scaly"*) then is edible.
* If (Odor is any of [a *"almond"*, l *"anise"*, n *"none"*]) & (stalk.surface.above.ring = k *"silky"*) then is poisonous.
* If (Odor is any of [a *"almond"*, l *"anise"*, n *"none"*]) & (stalk.surface.above.ring is any of [f *"fibrous"*, s *"smooth"*]) & (spore.print.color is any of [b *"buff"*, o *"orange"*, y *"yellow"*]) then is edible.
* If (Odor is any of [a *"almond"*, l *"anise"*, n *"none"*]) & (stalk.surface.above.ring is any of [f *"fibrous"*, s *"smooth"*]) & (spore.print.color = r *"green"*) then is poisonous.
* If (Odor is any of [a *"almond"*, l *"anise"*, n *"none"*]) & (stalk.surface.above.ring is any of [f *"fibrous"*, s *"smooth"*]) & (spore.print.color is any of [h *"chocolate"*, k *"black"*, n *"brown"*, u *"purple"*, w *"white"*]) & (cap.surface is any of [f *"fibrous"*, s *"smooth"*, y *"scaly"*]) then is edible.
* If (Odor is any of [a *"almond"*, l *"anise"*, n *"none"*]) & (stalk.surface.above.ring is any of [f *"fibrous"*, s *"smooth"*]) & (spore.print.color is any of [h *"chocolate"*, k *"black"*, n *"brown"*, u *"purple"*, w *"white"*]) & (cap.surface = g *"grooves"*) then is poisonous.

Next I try to validate the model.

```R
prediction <- predict( model, testInputs, type="class" )

# Check the accuracy of the model
sum( prediction == testOutput ) / length( prediction )
```
    [1] 0.9302068

```R
# Confusion matrix
table(prediction, testOutput)
```
              testOutput
    prediction    e    p
             e  537   45
             p  144 1982

I can see that the model **have a 93% of probability to hit**. It is a **high percentage** of hit and I are sospite that **maybe the model can be overfitted**.

To discard this possibility I decide to **use cross validation** technique to **be sure that the values of the dataset are independent between they**.

I decide **to do 10 iterations** reordering the dataset randomly to each iteration and choose 1/3 for made the test and 2/3 to made the training.

```R
Folds <- 10

# MODELS
Iter <- data.frame(iteracion = NULL, percentHits = NULL)
for (i in 1:Folds)
{
  # Reorder dataset random
  mushroomsFolds <- mushrooms[ sample( nrow( mushrooms )), ]

  # create dataset to test and training
  X <- mushroomsFolds[,2:22]
  y <- mushroomsFolds[,1]

  # calculate split 1/3 to test. This number is where the dataset must split the values
  split <- length(mushroomsFolds$class) - round(length(mushroomsFolds$class)/3)

  trainInputs <- X[1:split,]
  trainOutput <- y[1:split]
  testInputs <- X[ (split + 1):length(mushroomsFolds$class),]
  testOutput <- y[(split + 1):length(mushroomsFolds$class)]

  # Create the model  
  model <- C50::C5.0( trainInputs, trainOutput )
  # Launch the predict with the test data
  prediction <- predict( model, testInputs, type="class" )

  # Check the accuracy of the model
  percentHits <- sum( prediction == testOutput ) / length( prediction )

  Iter <- rbind(Iter, data.frame(Iter = i, percentHits = percentHits))  
}

# Plot
average  <- format(mean(Iter$percentHits, na.rm=TRUE)*100,digits = 4)
plot(Iter,type = "b", main = "% Prediction in each Iteration",  
     cex.axis = .7,cex.lab = .7,cex.main = .8,
     xlab ="No. Iteration", ylab="% Prediction")
abline(h = mean(Iter$percentHits), col = "blue", lty = 2)
legend("topright", legend = paste("Effectivity Prediction =", average, "%"),
       col = "blue", lty = 2, lwd = 1, cex=.7, bg=NULL)
```

![GitHub Logo](/images/cross_validation.png)

As you can see, the **mean of hit** to this 10 iterations is **near to be to 100%** and, I can sospite that, or this dataset has been modeled to be a perfect classification, or the observations of **this dataset is not representative of the reality**. Probably the second option is the most reasonable because **some of the attributes in this datset not have observations with all possible values** (as is the case of the attribute veil.type).

*NOTE*: Dataset extracted from https://www.kaggle.com/uciml/mushroom-classification/
