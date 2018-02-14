
========================================================
author: Abhimanyu Kumar
date: Jan 28th, 2018
autosize: true

========================================================

## Question 2.) Principal component Analysis.

I have taken data set from an ongoing hackathon and implemented it with the help of some reference.

PCA <- When we have too many data sets we always get confused with the idea that most of them are corelated and PCA helps us in dealing with such situation.
Basically its a method of extracting important variables from a set of large no. variables in a dataset. It is even more powerful when dealing with 3 or higher dimensional data.

PCA can only be applied on numerical data so if our data set is having categorical then we should convert them into numerical data.

Some basic data cleaning is also important before implementing the technique to improve its accuracy.

========================================================


```r
#load train and test file
train <- read.csv("train_Big.csv")
test <- read.csv("test_Big.csv")

#add a column
test$Item_Outlet_Sales <- 1

#combine the data set
combi <- rbind(train, test)

#impute missing values with median
combi$Item_Weight[is.na(combi$Item_Weight)] <- median(combi$Item_Weight, na.rm = TRUE)

#impute 0 with median
combi$Item_Visibility <- ifelse(combi$Item_Visibility == 0, median(combi$Item_Visibility),combi$Item_Visibility)

#find mode and impute
table(combi$Outlet_Size, combi$Outlet_Type)
```

```
        
         Grocery Store Supermarket Type1 Supermarket Type2
                   925              3091                 0
  High               0              1553                 0
  Medium             0              1550              1546
  Small            880              3100                 0
        
         Supermarket Type3
                         0
  High                   0
  Medium              1559
  Small                  0
```

```r
levels(combi$Outlet_Size)[1] <- "Other"
```

========================================================

As this is a unsupervised technique so we need to remove dependent and other identifier variable. We will do that in the below steps.


```r
#remove the dependent and identifier variables
my_data <- subset(combi, select = -c(Item_Outlet_Sales, Item_Identifier,Outlet_Identifier))
```

Let’s check the available variables ( a.k.a predictors) in the data set.

```r
#check available variables
 colnames(my_data)
```

```
[1] "Item_Weight"               "Item_Fat_Content"         
[3] "Item_Visibility"           "Item_Type"                
[5] "Item_MRP"                  "Outlet_Establishment_Year"
[7] "Outlet_Size"               "Outlet_Location_Type"     
[9] "Outlet_Type"              
```

========================================================
Since PCA works on numeric variables, let’s see if we have any variable other than numeric.

```r
#check variable class
str(my_data)
```

```
'data.frame':	14204 obs. of  9 variables:
 $ Item_Weight              : num  9.3 5.92 17.5 19.2 8.93 ...
 $ Item_Fat_Content         : Factor w/ 5 levels "LF","low fat",..: 3 5 3 5 3 5 5 3 5 5 ...
 $ Item_Visibility          : num  0.016 0.0193 0.0168 0.054 0.054 ...
 $ Item_Type                : Factor w/ 16 levels "Baking Goods",..: 5 15 11 7 10 1 14 14 6 6 ...
 $ Item_MRP                 : num  249.8 48.3 141.6 182.1 53.9 ...
 $ Outlet_Establishment_Year: int  1999 2009 1999 1998 1987 2009 1987 1985 2002 2007 ...
 $ Outlet_Size              : Factor w/ 4 levels "Other","High",..: 3 3 3 1 2 3 2 3 1 1 ...
 $ Outlet_Location_Type     : Factor w/ 3 levels "Tier 1","Tier 2",..: 1 3 1 3 3 3 3 3 2 2 ...
 $ Outlet_Type              : Factor w/ 4 levels "Grocery Store",..: 2 3 2 1 2 3 2 4 2 2 ...
```

We can see here that 6 out 9 variables we have are categorical. So, we will convert them into numeric variables in our further steps.

========================================================

I installed a package named dummies to create dummy data as i used one hot encoding to get rid of the categorical data. Further after that i created a dummy data frame with one column by encoding categorical data.


```r
#install package
install.packages("dummies", repos = "http://cran.us.r-project.org")
```

```

The downloaded binary packages are in
	/var/folders/mn/7jk0pyy53cjbzyt8ryfh3wzc0000gn/T//RtmpbK35S5/downloaded_packages
```

```r
#load library
library(dummies)
```


```r
#create a dummy data frame
new_my_data <- dummy.data.frame(my_data, names = c("Item_Fat_Content","Item_Type",
                                "Outlet_Establishment_Year","Outlet_Size",
                                "Outlet_Location_Type","Outlet_Type"))
```

========================================================

To check, if we now have a data set of integer values, simple write:


```r
#check the data set
str(new_my_data)
```

```
'data.frame':	14204 obs. of  44 variables:
 $ Item_Weight                   : num  9.3 5.92 17.5 19.2 8.93 ...
 $ Item_Fat_ContentLF            : int  0 0 0 0 0 0 0 0 0 0 ...
 $ Item_Fat_Contentlow fat       : int  0 0 0 0 0 0 0 0 0 0 ...
 $ Item_Fat_ContentLow Fat       : int  1 0 1 0 1 0 0 1 0 0 ...
 $ Item_Fat_Contentreg           : int  0 0 0 0 0 0 0 0 0 0 ...
 $ Item_Fat_ContentRegular       : int  0 1 0 1 0 1 1 0 1 1 ...
 $ Item_Visibility               : num  0.016 0.0193 0.0168 0.054 0.054 ...
 $ Item_TypeBaking Goods         : int  0 0 0 0 0 1 0 0 0 0 ...
 $ Item_TypeBreads               : int  0 0 0 0 0 0 0 0 0 0 ...
 $ Item_TypeBreakfast            : int  0 0 0 0 0 0 0 0 0 0 ...
 $ Item_TypeCanned               : int  0 0 0 0 0 0 0 0 0 0 ...
 $ Item_TypeDairy                : int  1 0 0 0 0 0 0 0 0 0 ...
 $ Item_TypeFrozen Foods         : int  0 0 0 0 0 0 0 0 1 1 ...
 $ Item_TypeFruits and Vegetables: int  0 0 0 1 0 0 0 0 0 0 ...
 $ Item_TypeHard Drinks          : int  0 0 0 0 0 0 0 0 0 0 ...
 $ Item_TypeHealth and Hygiene   : int  0 0 0 0 0 0 0 0 0 0 ...
 $ Item_TypeHousehold            : int  0 0 0 0 1 0 0 0 0 0 ...
 $ Item_TypeMeat                 : int  0 0 1 0 0 0 0 0 0 0 ...
 $ Item_TypeOthers               : int  0 0 0 0 0 0 0 0 0 0 ...
 $ Item_TypeSeafood              : int  0 0 0 0 0 0 0 0 0 0 ...
 $ Item_TypeSnack Foods          : int  0 0 0 0 0 0 1 1 0 0 ...
 $ Item_TypeSoft Drinks          : int  0 1 0 0 0 0 0 0 0 0 ...
 $ Item_TypeStarchy Foods        : int  0 0 0 0 0 0 0 0 0 0 ...
 $ Item_MRP                      : num  249.8 48.3 141.6 182.1 53.9 ...
 $ Outlet_Establishment_Year1985 : int  0 0 0 0 0 0 0 1 0 0 ...
 $ Outlet_Establishment_Year1987 : int  0 0 0 0 1 0 1 0 0 0 ...
 $ Outlet_Establishment_Year1997 : int  0 0 0 0 0 0 0 0 0 0 ...
 $ Outlet_Establishment_Year1998 : int  0 0 0 1 0 0 0 0 0 0 ...
 $ Outlet_Establishment_Year1999 : int  1 0 1 0 0 0 0 0 0 0 ...
 $ Outlet_Establishment_Year2002 : int  0 0 0 0 0 0 0 0 1 0 ...
 $ Outlet_Establishment_Year2004 : int  0 0 0 0 0 0 0 0 0 0 ...
 $ Outlet_Establishment_Year2007 : int  0 0 0 0 0 0 0 0 0 1 ...
 $ Outlet_Establishment_Year2009 : int  0 1 0 0 0 1 0 0 0 0 ...
 $ Outlet_SizeOther              : int  0 0 0 1 0 0 0 0 1 1 ...
 $ Outlet_SizeHigh               : int  0 0 0 0 1 0 1 0 0 0 ...
 $ Outlet_SizeMedium             : int  1 1 1 0 0 1 0 1 0 0 ...
 $ Outlet_SizeSmall              : int  0 0 0 0 0 0 0 0 0 0 ...
 $ Outlet_Location_TypeTier 1    : int  1 0 1 0 0 0 0 0 0 0 ...
 $ Outlet_Location_TypeTier 2    : int  0 0 0 0 0 0 0 0 1 1 ...
 $ Outlet_Location_TypeTier 3    : int  0 1 0 1 1 1 1 1 0 0 ...
 $ Outlet_TypeGrocery Store      : int  0 0 0 1 0 0 0 0 0 0 ...
 $ Outlet_TypeSupermarket Type1  : int  1 0 1 0 1 0 1 0 1 1 ...
 $ Outlet_TypeSupermarket Type2  : int  0 1 0 0 0 1 0 0 0 0 ...
 $ Outlet_TypeSupermarket Type3  : int  0 0 0 0 0 0 0 1 0 0 ...
 - attr(*, "dummies")=List of 6
  ..$ Item_Fat_Content         : int  2 3 4 5 6
  ..$ Item_Type                : int  8 9 10 11 12 13 14 15 16 17 ...
  ..$ Outlet_Establishment_Year: int  25 26 27 28 29 30 31 32 33
  ..$ Outlet_Size              : int  34 35 36 37
  ..$ Outlet_Location_Type     : int  38 39 40
  ..$ Outlet_Type              : int  41 42 43 44
```

========================================================

Now we have all the dataset into numerical value so again i will divide them into two parts in which they originally were i.e. test and train.


```r
#divide the new data
pca.train <- new_my_data[1:nrow(train),]
pca.test <- new_my_data[-(1:nrow(train)),]
```

## Now, I am all set to apply PCA here. 

========================================================

We have 5 function to perform PCA but Here I will be using base R function prcomp() to perform PCA. It is having a bydefault propert that it centers the variable to have mean equals to zero. We will also normalize the variables to have standard deviation equals to 1 with parameter scale = T.


```r
#principal component analysis
prin_comp <- prcomp(pca.train, scale. = T)
names(prin_comp)
```

```
[1] "sdev"     "rotation" "center"   "scale"    "x"       
```

========================================================

The prcomp() function results in 5 useful measures, which i will be explainign one by one in my further steps :-

## Step 1.) Here center and scale refers to respective mean and standard deviation of the variables that are used for normalization prior to implementing PCA.

## Mean

```r
#outputs the mean of variables
prin_comp$center
```

```
                   Item_Weight             Item_Fat_ContentLF 
                  1.281342e+01                   3.707615e-02 
       Item_Fat_Contentlow fat        Item_Fat_ContentLow Fat 
                  1.314091e-02                   5.970902e-01 
           Item_Fat_Contentreg        Item_Fat_ContentRegular 
                  1.372756e-02                   3.389652e-01 
               Item_Visibility          Item_TypeBaking Goods 
                  6.946593e-02                   7.602957e-02 
               Item_TypeBreads             Item_TypeBreakfast 
                  2.944972e-02                   1.290625e-02 
               Item_TypeCanned                 Item_TypeDairy 
                  7.614690e-02                   8.001877e-02 
         Item_TypeFrozen Foods Item_TypeFruits and Vegetables 
                  1.004341e-01                   1.445500e-01 
          Item_TypeHard Drinks    Item_TypeHealth and Hygiene 
                  2.510853e-02                   6.101138e-02 
            Item_TypeHousehold                  Item_TypeMeat 
                  1.067699e-01                   4.986507e-02 
               Item_TypeOthers               Item_TypeSeafood 
                  1.982870e-02                   7.509093e-03 
          Item_TypeSnack Foods           Item_TypeSoft Drinks 
                  1.407955e-01                   5.221166e-02 
        Item_TypeStarchy Foods                       Item_MRP 
                  1.736478e-02                   1.409928e+02 
 Outlet_Establishment_Year1985  Outlet_Establishment_Year1987 
                  1.716532e-01                   1.093512e-01 
 Outlet_Establishment_Year1997  Outlet_Establishment_Year1998 
                  1.091165e-01                   6.511792e-02 
 Outlet_Establishment_Year1999  Outlet_Establishment_Year2002 
                  1.091165e-01                   1.089992e-01 
 Outlet_Establishment_Year2004  Outlet_Establishment_Year2007 
                  1.091165e-01                   1.086472e-01 
 Outlet_Establishment_Year2009               Outlet_SizeOther 
                  1.088818e-01                   2.827643e-01 
               Outlet_SizeHigh              Outlet_SizeMedium 
                  1.093512e-01                   3.277015e-01 
              Outlet_SizeSmall     Outlet_Location_TypeTier 1 
                  2.801830e-01                   2.801830e-01 
    Outlet_Location_TypeTier 2     Outlet_Location_TypeTier 3 
                  3.267629e-01                   3.930541e-01 
      Outlet_TypeGrocery Store   Outlet_TypeSupermarket Type1 
                  1.270679e-01                   6.543471e-01 
  Outlet_TypeSupermarket Type2   Outlet_TypeSupermarket Type3 
                  1.088818e-01                   1.097032e-01 
```

========================================================
## Standard deviation.

```r
#outputs the standard deviation of variables
prin_comp$scale
```

```
                   Item_Weight             Item_Fat_ContentLF 
                    4.22724041                     0.18895951 
       Item_Fat_Contentlow fat        Item_Fat_ContentLow Fat 
                    0.11388481                     0.49051169 
           Item_Fat_Contentreg        Item_Fat_ContentRegular 
                    0.11636453                     0.47338575 
               Item_Visibility          Item_TypeBaking Goods 
                    0.04889103                     0.26506097 
               Item_TypeBreads             Item_TypeBreakfast 
                    0.16907333                     0.11287682 
               Item_TypeCanned                 Item_TypeDairy 
                    0.26524857                     0.27133818 
         Item_TypeFrozen Foods Item_TypeFruits and Vegetables 
                    0.30059559                     0.35166722 
          Item_TypeHard Drinks    Item_TypeHealth and Hygiene 
                    0.15646394                     0.23936523 
            Item_TypeHousehold                  Item_TypeMeat 
                    0.30883862                     0.21767890 
               Item_TypeOthers               Item_TypeSeafood 
                    0.13941952                     0.08633412 
          Item_TypeSnack Foods           Item_TypeSoft Drinks 
                    0.34783088                     0.22246665 
        Item_TypeStarchy Foods                       Item_MRP 
                    0.13063401                    62.27506651 
 Outlet_Establishment_Year1985  Outlet_Establishment_Year1987 
                    0.37710084                     0.31209761 
 Outlet_Establishment_Year1997  Outlet_Establishment_Year1998 
                    0.31180363                     0.24674829 
 Outlet_Establishment_Year1999  Outlet_Establishment_Year2002 
                    0.31180363                     0.31165647 
 Outlet_Establishment_Year2004  Outlet_Establishment_Year2007 
                    0.31180363                     0.31121430 
 Outlet_Establishment_Year2009               Outlet_SizeOther 
                    0.31150919                     0.45036923 
               Outlet_SizeHigh              Outlet_SizeMedium 
                    0.31209761                     0.46940290 
              Outlet_SizeSmall     Outlet_Location_TypeTier 1 
                    0.44911487                     0.44911487 
    Outlet_Location_TypeTier 2     Outlet_Location_TypeTier 3 
                    0.46905726                     0.48845733 
      Outlet_TypeGrocery Store   Outlet_TypeSupermarket Type1 
                    0.33306860                     0.47560858 
  Outlet_TypeSupermarket Type2   Outlet_TypeSupermarket Type3 
                    0.31150919                     0.31253773 
```

========================================================

## Step 2.) The rotation measure provides the principal component loading. Each column of rotation matrix contains the principal component loading vector. This is the most important measure we should be interested in.


```r
prin_comp$rotation
```

```
                                         PC1           PC2          PC3
Item_Weight                     0.0010441240 -0.0004384151 -0.021454235
Item_Fat_ContentLF             -0.0091725762  0.0019042858  0.008383420
Item_Fat_Contentlow fat        -0.0034287693 -0.0055492764 -0.003302283
Item_Fat_ContentLow Fat         0.0078360026 -0.0065124968 -0.021090747
Item_Fat_Contentreg            -0.0012517669 -0.0033104683 -0.006895653
Item_Fat_ContentRegular        -0.0033255317  0.0081367517  0.020996872
Item_Visibility                -0.0220000614  0.0187067072  0.163246663
Item_TypeBaking Goods           0.0020985501  0.0021432744  0.004446981
Item_TypeBreads                -0.0014593729 -0.0014755583  0.008779674
Item_TypeBreakfast             -0.0041558943  0.0013373054  0.009056857
Item_TypeCanned                 0.0007185925  0.0018660124  0.002503213
Item_TypeDairy                  0.0007705650  0.0083753476 -0.002652466
Item_TypeFrozen Foods           0.0047693050  0.0075665792 -0.002407384
Item_TypeFruits and Vegetables -0.0042284678 -0.0056042888 -0.001823856
Item_TypeHard Drinks            0.0017679688 -0.0071341635 -0.004325508
Item_TypeHealth and Hygiene    -0.0029676602 -0.0106048406 -0.010642563
Item_TypeHousehold              0.0022643056 -0.0042437346 -0.010402040
Item_TypeMeat                  -0.0111496574  0.0061744455  0.017570630
Item_TypeOthers                 0.0007948501  0.0093427539  0.003622752
Item_TypeSeafood                0.0001973747  0.0034001317  0.011209485
Item_TypeSnack Foods           -0.0009039454  0.0022045145  0.002898289
Item_TypeSoft Drinks            0.0075602653 -0.0058181850 -0.002596304
Item_TypeStarchy Foods          0.0036699523 -0.0075632647 -0.011911009
Item_MRP                        0.0020549197  0.0029926096 -0.006868498
Outlet_Establishment_Year1985  -0.2245166654  0.1294106363  0.236919922
Outlet_Establishment_Year1987  -0.0366530891 -0.1399494332 -0.481852069
Outlet_Establishment_Year1997   0.1056693202  0.3344969170 -0.047826794
Outlet_Establishment_Year1998  -0.0609422946 -0.1492292115  0.280348767
Outlet_Establishment_Year1999  -0.0094452317  0.2252021952 -0.090424680
Outlet_Establishment_Year2002   0.1806307642 -0.2136709043  0.083864703
Outlet_Establishment_Year2004   0.1712888260  0.0674229277 -0.008057850
Outlet_Establishment_Year2007   0.1802021523 -0.2130094442  0.084199637
Outlet_Establishment_Year2009  -0.2717273200 -0.0993753888 -0.047686334
Outlet_SizeOther                0.2161311654 -0.3768142808  0.269815694
Outlet_SizeHigh                -0.0366530891 -0.1399494332 -0.481852069
Outlet_SizeMedium              -0.3495034662  0.0908492736 -0.011809182
Outlet_SizeSmall                0.1740277629  0.3801667215  0.076620614
Outlet_Location_TypeTier 1      0.0485508779  0.4897069556  0.019436410
Outlet_Location_TypeTier 2      0.3534423717 -0.2384800339  0.106231468
Outlet_Location_TypeTier 3     -0.3840450727 -0.2212555277 -0.119883188
Outlet_TypeGrocery Store       -0.0697619219  0.0258097279  0.363324766
Outlet_TypeSupermarket Type1    0.3876052603  0.0399015911 -0.302062565
Outlet_TypeSupermarket Type2   -0.2717273200 -0.0993753888 -0.047686334
Outlet_TypeSupermarket Type3   -0.2446658990  0.0108223672  0.120005383
                                         PC4           PC5           PC6
Item_Weight                     0.0009241751 -0.0283406635  0.0242005816
Item_Fat_ContentLF              0.0067791081  0.0250978627  0.0259479814
Item_Fat_Contentlow fat         0.0200639450  0.0140234287 -0.0250123125
Item_Fat_ContentLow Fat        -0.0263226605 -0.6221332323  0.0269656291
Item_Fat_Contentreg            -0.0128246294  0.0610392876  0.0239620544
Item_Fat_ContentRegular         0.0228945400  0.6162443164 -0.0381715854
Item_Visibility                 0.1701091355  0.0629439793  0.1254898938
Item_TypeBaking Goods           0.0120375794  0.0893310856 -0.0004386544
Item_TypeBreads                 0.0015135101  0.0356553884 -0.0098616126
Item_TypeBreakfast              0.0157388360  0.0649245210  0.0064607211
Item_TypeCanned                -0.0141072217  0.0786389740 -0.0023081596
Item_TypeDairy                  0.0128859339  0.0258746644  0.0140944696
Item_TypeFrozen Foods          -0.0010100563  0.0853139171  0.0044293303
Item_TypeFruits and Vegetables  0.0065244777  0.1319497330 -0.0142394765
Item_TypeHard Drinks           -0.0100457962 -0.1188526557 -0.0022037789
Item_TypeHealth and Hygiene    -0.0055238077 -0.1999928197  0.0036626271
Item_TypeHousehold             -0.0009345115 -0.2819544242  0.0213422906
Item_TypeMeat                   0.0106117863  0.1160112579 -0.0172018319
Item_TypeOthers                -0.0029212769 -0.1069808347  0.0199205458
Item_TypeSeafood                0.0003114030  0.0166938955  0.0003085162
Item_TypeSnack Foods           -0.0112771087  0.0621428911 -0.0184959946
Item_TypeSoft Drinks           -0.0048315359 -0.0921839513  0.0044959701
Item_TypeStarchy Foods         -0.0054966110  0.0283021289  0.0058860125
Item_MRP                       -0.0017418472  0.0040989851  0.0130229316
Outlet_Establishment_Year1985   0.1600870148 -0.0387459717 -0.3982890041
Outlet_Establishment_Year1987   0.3295014435  0.0088387444 -0.0066125717
Outlet_Establishment_Year1997   0.0456657750  0.0007307698  0.1782403558
Outlet_Establishment_Year1998   0.3089324127 -0.0179418651  0.2621653005
Outlet_Establishment_Year1999  -0.1797652167  0.0026710209 -0.1340219010
Outlet_Establishment_Year2002  -0.1130818260 -0.0051777794 -0.0976501784
Outlet_Establishment_Year2004  -0.0668370473  0.0089244615  0.0554628241
Outlet_Establishment_Year2007  -0.1114943316  0.0104566965 -0.0958364985
Outlet_Establishment_Year2009  -0.3429756411  0.0346563026  0.3747820789
Outlet_SizeOther                0.0139601685 -0.0061872331  0.0098360182
Outlet_SizeHigh                 0.3295014435  0.0088387444 -0.0066125717
Outlet_SizeMedium              -0.3251183953  0.0022983710 -0.1816618959
Outlet_SizeSmall                0.0968297368 -0.0023398771  0.1845998747
Outlet_Location_TypeTier 1      0.0184279397 -0.0066814065  0.0530477292
Outlet_Location_TypeTier 2     -0.1935400103  0.0094301163 -0.0915996671
Outlet_Location_TypeTier 3      0.1689095028 -0.0029123229  0.0391865647
Outlet_TypeGrocery Store        0.3792537004 -0.0254858437  0.2243558498
Outlet_TypeSupermarket Type1   -0.0625670510  0.0173304404 -0.0656880113
Outlet_TypeSupermarket Type2   -0.3429756411  0.0346563026  0.3747820789
Outlet_TypeSupermarket Type3    0.0328920096 -0.0337550567 -0.5126810371
                                         PC7         PC8          PC9
Item_Weight                    -0.0130130642  0.07307716 -0.395469211
Item_Fat_ContentLF              0.0259987717 -0.04268011  0.057000975
Item_Fat_Contentlow fat         0.0034789729 -0.01507710  0.098453645
Item_Fat_ContentLow Fat        -0.0027067382  0.02443334 -0.017348408
Item_Fat_Contentreg            -0.0317668924  0.02402929  0.005261382
Item_Fat_ContentRegular        -0.0006013935 -0.01056037 -0.029755633
Item_Visibility                -0.0492471259  0.15447029  0.049202313
Item_TypeBaking Goods          -0.0009862694 -0.11233989  0.369379956
Item_TypeBreads                 0.0109158367 -0.02066127  0.138351791
Item_TypeBreakfast             -0.0131647099  0.03474490  0.018932325
Item_TypeCanned                 0.0225625262 -0.03191133  0.164037941
Item_TypeDairy                 -0.0115640910  0.04490857 -0.153666181
Item_TypeFrozen Foods          -0.0048745810 -0.11124306  0.072173570
Item_TypeFruits and Vegetables -0.0117451068  0.06702915 -0.381109231
Item_TypeHard Drinks           -0.0234030706  0.03336807  0.183904620
Item_TypeHealth and Hygiene    -0.0010922997 -0.09538780  0.132477662
Item_TypeHousehold              0.0076141709  0.04342297 -0.243895605
Item_TypeMeat                  -0.0007253870 -0.02417547  0.018845960
Item_TypeOthers                 0.0006872056 -0.05095303 -0.004116611
Item_TypeSeafood                0.0055547061  0.01936073  0.027352873
Item_TypeSnack Foods            0.0067409084  0.15717448 -0.175571344
Item_TypeSoft Drinks            0.0067529200 -0.04447977  0.284797924
Item_TypeStarchy Foods         -0.0003233152  0.04298609 -0.047943225
Item_MRP                        0.0139577952  0.11893415 -0.427311346
Outlet_Establishment_Year1985   0.1866013094 -0.09089381 -0.028676819
Outlet_Establishment_Year1987  -0.0038905892  0.01136913  0.013385987
Outlet_Establishment_Year1997   0.0128975359 -0.52421708 -0.138298407
Outlet_Establishment_Year1998  -0.1866882762  0.17544033  0.036514652
Outlet_Establishment_Year1999  -0.4872394462  0.40631453  0.108153411
Outlet_Establishment_Year2002  -0.0893145712 -0.14805441 -0.076394860
Outlet_Establishment_Year2004   0.4936363257  0.48805338  0.121031771
Outlet_Establishment_Year2007  -0.0904621341 -0.18774655 -0.011412262
Outlet_Establishment_Year2009   0.0863032299 -0.07513292 -0.010759953
Outlet_SizeOther               -0.2265998194 -0.13607041 -0.040745865
Outlet_SizeHigh                -0.0038905892  0.01136913  0.013385987
Outlet_SizeMedium              -0.1441569836  0.14293668  0.034860560
Outlet_SizeSmall                0.3806053794 -0.02084377 -0.004877814
Outlet_Location_TypeTier 1     -0.3003799201 -0.07759199 -0.013818777
Outlet_Location_TypeTier 2      0.2087783991  0.10149143  0.022124237
Outlet_Location_TypeTier 3      0.0756996829 -0.02611809 -0.008539775
Outlet_TypeGrocery Store       -0.0992845701  0.13572076  0.036638185
Outlet_TypeSupermarket Type1   -0.1076236853  0.03025834  0.010840740
Outlet_TypeSupermarket Type2    0.0863032299 -0.07513292 -0.010759953
Outlet_TypeSupermarket Type3    0.1835652641 -0.11579676 -0.044817481
                                        PC10         PC11          PC12
Item_Weight                     0.0237336129 -0.215496169  0.0117520599
Item_Fat_ContentLF              0.2158491225 -0.092224750  0.5977952571
Item_Fat_Contentlow fat        -0.0009120667  0.121690970  0.3137438273
Item_Fat_ContentLow Fat        -0.0472247360  0.021596579 -0.2234346965
Item_Fat_Contentreg            -0.0131910398 -0.105177191 -0.0332568757
Item_Fat_ContentRegular        -0.0337644703  0.011013237 -0.0744055006
Item_Visibility                 0.0313925476  0.051110372  0.0388264857
Item_TypeBaking Goods           0.0552617787  0.010598159 -0.0045854315
Item_TypeBreads                 0.0036413539  0.190836775  0.0669675627
Item_TypeBreakfast             -0.0057494593 -0.021042258 -0.0032317753
Item_TypeCanned                -0.0105611580  0.134590268  0.0593486618
Item_TypeDairy                 -0.0178705524 -0.065875051  0.3228304352
Item_TypeFrozen Foods          -0.0763362251 -0.456454921 -0.3150249134
Item_TypeFruits and Vegetables  0.5979630265  0.159446368 -0.3098223500
Item_TypeHard Drinks            0.0105832796  0.159062296 -0.0629215602
Item_TypeHealth and Hygiene     0.0552277089 -0.095490211 -0.1159003004
Item_TypeHousehold              0.0669459514 -0.066768668  0.3078879490
Item_TypeMeat                  -0.0227060348 -0.103791636 -0.0198768502
Item_TypeOthers                 0.0363678469 -0.002568400 -0.0238974380
Item_TypeSeafood                0.0313245576  0.065240817  0.0864020868
Item_TypeSnack Foods           -0.7328811912  0.198082416 -0.0236042805
Item_TypeSoft Drinks            0.0825136926  0.051968135  0.1049565208
Item_TypeStarchy Foods          0.0296802106 -0.106709074  0.1387748137
Item_MRP                       -0.0976127733  0.049108771  0.1270008456
Outlet_Establishment_Year1985  -0.0075851530  0.011046319  0.0073889335
Outlet_Establishment_Year1987  -0.0056495434  0.010101672  0.0005787050
Outlet_Establishment_Year1997  -0.0528460215  0.052236007  0.0067948399
Outlet_Establishment_Year1998   0.0088458572 -0.042714687 -0.0294703912
Outlet_Establishment_Year1999   0.0457747598 -0.045639354  0.0048133270
Outlet_Establishment_Year2002   0.0399929591  0.522556807 -0.0420962846
Outlet_Establishment_Year2004   0.0566457017 -0.067043605 -0.0340147421
Outlet_Establishment_Year2007  -0.0726173145 -0.475512346  0.0774155941
Outlet_Establishment_Year2009  -0.0092489447  0.023103989  0.0010207522
Outlet_SizeOther               -0.0176583609  0.009619196  0.0082187947
Outlet_SizeHigh                -0.0056495434  0.010101672  0.0005787050
Outlet_SizeMedium               0.0221450915 -0.021579412  0.0067043502
Outlet_SizeSmall               -0.0015118155  0.005888340 -0.0156511097
Outlet_Location_TypeTier 1     -0.0090591031  0.020748509  0.0113057639
Outlet_Location_TypeTier 2      0.0160467676 -0.012860846  0.0007831469
Outlet_Location_TypeTier 3     -0.0070799938 -0.006727283 -0.0111471916
Outlet_TypeGrocery Store        0.0009576622 -0.009842333 -0.0174548139
Outlet_TypeSupermarket Type1    0.0074826210 -0.001730216  0.0087622930
Outlet_TypeSupermarket Type2   -0.0092489447  0.023103989  0.0010207522
Outlet_TypeSupermarket Type3   -0.0031888460 -0.009906092  0.0042499025
                                        PC13         PC14          PC15
Item_Weight                     0.0703114620  0.077844306 -0.2870737316
Item_Fat_ContentLF              0.3103571348 -0.236712971 -0.0142537998
Item_Fat_Contentlow fat         0.0794716150  0.104735507  0.1305923253
Item_Fat_ContentLow Fat        -0.0915067989  0.083422880  0.0091913886
Item_Fat_Contentreg             0.2561183140 -0.162531814  0.0670735142
Item_Fat_ContentRegular        -0.1111429179  0.022802593 -0.0517391008
Item_Visibility                -0.0402657765  0.135306200  0.1155079692
Item_TypeBaking Goods           0.1147291447  0.061775918 -0.4491832495
Item_TypeBreads                -0.1187941609 -0.092453034  0.1076849785
Item_TypeBreakfast             -0.0785136428  0.045303864  0.0124943991
Item_TypeCanned                -0.2954597190 -0.021837063  0.2884302757
Item_TypeDairy                 -0.3711284523  0.610502974 -0.1276981500
Item_TypeFrozen Foods          -0.2615651385 -0.348029940  0.1081184504
Item_TypeFruits and Vegetables  0.2963583622  0.100403057  0.2137130703
Item_TypeHard Drinks           -0.0641222347  0.069206675  0.2293978674
Item_TypeHealth and Hygiene     0.2445843467  0.227174533 -0.2323224930
Item_TypeHousehold             -0.1914404873 -0.412164260 -0.0557044124
Item_TypeMeat                  -0.1373903229 -0.124515300 -0.3134474529
Item_TypeOthers                 0.0773049997  0.043115775 -0.1597751452
Item_TypeSeafood               -0.0320398284 -0.032795546 -0.0469522328
Item_TypeSnack Foods            0.4147981772 -0.003834790  0.0590110202
Item_TypeSoft Drinks            0.1135381368  0.022230962  0.2990764672
Item_TypeStarchy Foods          0.1147000517 -0.107250377 -0.0432293389
Item_MRP                       -0.1801973797 -0.058183616  0.1609552493
Outlet_Establishment_Year1985   0.0028086201  0.033818663  0.0173450345
Outlet_Establishment_Year1987  -0.0042785607  0.011241214  0.0100838153
Outlet_Establishment_Year1997   0.0211747995 -0.012641187  0.0348200896
Outlet_Establishment_Year1998   0.0054100506 -0.075240446 -0.0408575466
Outlet_Establishment_Year1999  -0.0125843519 -0.027252925 -0.0329936414
Outlet_Establishment_Year2002  -0.1257812112 -0.179766096 -0.2500980237
Outlet_Establishment_Year2004  -0.0081640109 -0.028122917 -0.0498323836
Outlet_Establishment_Year2007   0.1258723996  0.227822335  0.2872491706
Outlet_Establishment_Year2009  -0.0037381006  0.027722143  0.0125537520
Outlet_SizeOther                0.0029035814 -0.008191386  0.0030416241
Outlet_SizeHigh                -0.0042785607  0.011241214  0.0100838153
Outlet_SizeMedium              -0.0089196223 -0.009318913 -0.0119555985
Outlet_SizeSmall                0.0093841057  0.010142432  0.0024381381
Outlet_Location_TypeTier 1      0.0063152285  0.010746435  0.0141286459
Outlet_Location_TypeTier 2     -0.0054850556  0.013020746 -0.0087122295
Outlet_Location_TypeTier 3     -0.0005393672 -0.022384471 -0.0046244582
Outlet_TypeGrocery Store        0.0044815122 -0.003902915 -0.0129271803
Outlet_TypeSupermarket Type1   -0.0025853213 -0.005936179 -0.0007777544
Outlet_TypeSupermarket Type2   -0.0037381006  0.027722143  0.0125537520
Outlet_TypeSupermarket Type3    0.0028841404 -0.014438149  0.0024474975
                                        PC16         PC17          PC18
Item_Weight                    -0.0553085196  0.042201243  0.1514502960
Item_Fat_ContentLF             -0.2367281494  0.084720289 -0.0373416192
Item_Fat_Contentlow fat        -0.0571170973 -0.045290533  0.1850834127
Item_Fat_ContentLow Fat         0.0648381747 -0.029123251 -0.0317575263
Item_Fat_Contentreg            -0.0631666763 -0.145960072  0.3467826747
Item_Fat_ContentRegular         0.0565781339  0.043134103 -0.0819583194
Item_Visibility                 0.0053234546 -0.199027545  0.0310635762
Item_TypeBaking Goods           0.3946953297 -0.464658889  0.0705330020
Item_TypeBreads                -0.0895792268  0.001884972 -0.1149852792
Item_TypeBreakfast              0.0622774247 -0.036520040 -0.0544305292
Item_TypeCanned                 0.2529124684  0.397288283  0.5754669817
Item_TypeDairy                 -0.2638409605 -0.160567913 -0.0140304283
Item_TypeFrozen Foods          -0.4607787957 -0.223130307  0.0572662487
Item_TypeFruits and Vegetables  0.0803651908 -0.045948557 -0.0499173373
Item_TypeHard Drinks           -0.0426865836 -0.108637605 -0.0312941903
Item_TypeHealth and Hygiene    -0.2103200265  0.415356575  0.1852360182
Item_TypeHousehold              0.3553825196 -0.147108819 -0.0394325585
Item_TypeMeat                   0.1675509865  0.497771864 -0.3752621501
Item_TypeOthers                -0.1294405551  0.037406303  0.0624447966
Item_TypeSeafood               -0.0964638994  0.006524230 -0.0473124276
Item_TypeSnack Foods            0.0002223435 -0.023530472 -0.0562970411
Item_TypeSoft Drinks           -0.1392033964  0.038387928 -0.4543670704
Item_TypeStarchy Foods         -0.1048067397 -0.023686786  0.2209162619
Item_MRP                        0.0154046712  0.006521334 -0.0485167724
Outlet_Establishment_Year1985  -0.0046723398 -0.038581190  0.0180114500
Outlet_Establishment_Year1987   0.0013699663 -0.009009871 -0.0009422203
Outlet_Establishment_Year1997   0.0212371369  0.004581048 -0.0152783733
Outlet_Establishment_Year1998  -0.0060159709  0.079987377 -0.0048680674
Outlet_Establishment_Year1999  -0.0115752477  0.028365071  0.0024455426
Outlet_Establishment_Year2002  -0.2664658309 -0.054553360  0.0659554293
Outlet_Establishment_Year2004  -0.0188649347  0.027720044  0.0043294607
Outlet_Establishment_Year2007   0.2770984998  0.005859491 -0.0650335886
Outlet_Establishment_Year2009   0.0080162543 -0.019625006 -0.0095071114
Outlet_SizeOther                0.0037897485  0.010121469 -0.0019653520
Outlet_SizeHigh                 0.0013699663 -0.009009871 -0.0009422203
Outlet_SizeMedium              -0.0055734026 -0.001951669  0.0003251860
Outlet_SizeSmall                0.0010728259 -0.001848791  0.0022857304
Outlet_Location_TypeTier 1      0.0061337824 -0.001400973  0.0009777966
Outlet_Location_TypeTier 2     -0.0057369082 -0.013932499  0.0035517940
Outlet_Location_TypeTier 3     -0.0001306858  0.014667274 -0.0043097680
Outlet_TypeGrocery Store       -0.0052309637  0.026525559  0.0097255608
Outlet_TypeSupermarket Type1    0.0015753224  0.001946182 -0.0055284812
Outlet_TypeSupermarket Type2    0.0080162543 -0.019625006 -0.0095071114
Outlet_TypeSupermarket Type3   -0.0048125519 -0.011669252  0.0075244253
                                       PC19         PC20          PC21
Item_Weight                     0.110902393 -0.061771146 -0.0771521568
Item_Fat_ContentLF              0.063876023 -0.136846539 -0.1048100935
Item_Fat_Contentlow fat        -0.398005569 -0.104284401 -0.2699571311
Item_Fat_ContentLow Fat         0.059711498  0.044134561  0.0574315524
Item_Fat_Contentreg            -0.031616109  0.461650904  0.1135300201
Item_Fat_ContentRegular         0.016153031 -0.079498470  0.0193651083
Item_Visibility                 0.107430533 -0.011532702  0.0435051224
Item_TypeBaking Goods           0.021369740 -0.039312530  0.0099421529
Item_TypeBreads                -0.454686686  0.009907178  0.4239007211
Item_TypeBreakfast              0.128712951 -0.059080634  0.3290109415
Item_TypeCanned                 0.285864543 -0.082452692 -0.0524856591
Item_TypeDairy                  0.028053896  0.124345447  0.0261059184
Item_TypeFrozen Foods          -0.044605684 -0.178132222 -0.1158251823
Item_TypeFruits and Vegetables -0.059734126 -0.049509433 -0.0538451202
Item_TypeHard Drinks           -0.298041284  0.321941118 -0.4353569606
Item_TypeHealth and Hygiene    -0.252976141 -0.235804001  0.2659656229
Item_TypeHousehold             -0.076783647 -0.148863489  0.0277771393
Item_TypeMeat                  -0.137365655  0.381442414 -0.1960060281
Item_TypeOthers                 0.273514670  0.084508106 -0.3957865066
Item_TypeSeafood                0.041910726 -0.187730446 -0.0834220813
Item_TypeSnack Foods            0.038348911 -0.105101277 -0.0547209495
Item_TypeSoft Drinks            0.456183701  0.064510559  0.1337625651
Item_TypeStarchy Foods          0.090983313  0.536972159  0.3012075475
Item_MRP                       -0.070311292  0.066042317  0.0882123904
Outlet_Establishment_Year1985   0.023987273  0.013191317  0.0015831114
Outlet_Establishment_Year1987   0.012473461 -0.003417763 -0.0037196424
Outlet_Establishment_Year1997  -0.012359415  0.045552610  0.0184904672
Outlet_Establishment_Year1998  -0.036204265 -0.006980947 -0.0049175758
Outlet_Establishment_Year1999  -0.001885226 -0.042649251 -0.0037793657
Outlet_Establishment_Year2002   0.103890781  0.018819691  0.0180343155
Outlet_Establishment_Year2004  -0.022306242 -0.018826219 -0.0107580406
Outlet_Establishment_Year2007  -0.076718188 -0.006458838 -0.0131238909
Outlet_Establishment_Year2009  -0.003566367 -0.003452978 -0.0031827152
Outlet_SizeOther               -0.000956779  0.004735356  0.0007166678
Outlet_SizeHigh                 0.012473461 -0.003417763 -0.0037196424
Outlet_SizeMedium               0.004721205 -0.013266453  0.0041366039
Outlet_SizeSmall               -0.012643048  0.011492224 -0.0024572946
Outlet_Location_TypeTier 1      0.001534499 -0.005047215  0.0023877380
Outlet_Location_TypeTier 2      0.003298679 -0.004295612 -0.0038763445
Outlet_Location_TypeTier 3     -0.004578569  0.008765693  0.0015269684
Outlet_TypeGrocery Store       -0.011416998 -0.014695453 -0.0141953029
Outlet_TypeSupermarket Type1    0.002099785 -0.004575791  0.0033806282
Outlet_TypeSupermarket Type2   -0.003566367 -0.003452978 -0.0031827152
Outlet_TypeSupermarket Type3    0.012526244  0.026065697  0.0131555279
                                       PC22          PC23          PC24
Item_Weight                    -0.013894517 -0.0515141674 -7.955829e-02
Item_Fat_ContentLF             -0.057146681  0.1099182005 -6.393484e-03
Item_Fat_Contentlow fat         0.071542178 -0.0897721117 -2.896805e-01
Item_Fat_ContentLow Fat        -0.011105649 -0.0122662220  6.739876e-02
Item_Fat_Contentreg             0.131010097 -0.0840573709 -1.176645e-01
Item_Fat_ContentRegular        -0.015096856  0.0110937281  3.132839e-02
Item_Visibility                -0.102056058  0.0989904616  1.704090e-02
Item_TypeBaking Goods           0.092819898 -0.0201696770  8.152832e-02
Item_TypeBreads                -0.333253031 -0.4648127073  1.505322e-01
Item_TypeBreakfast             -0.522303031  0.4525105007 -5.153052e-01
Item_TypeCanned                 0.054744193 -0.0551211422  3.863755e-02
Item_TypeDairy                  0.173553443 -0.0750647264 -1.170519e-02
Item_TypeFrozen Foods           0.072010845  0.0058050719 -5.282374e-02
Item_TypeFruits and Vegetables  0.042506490 -0.0360358905  8.652657e-03
Item_TypeHard Drinks           -0.102517563  0.4085329191 -6.010529e-03
Item_TypeHealth and Hygiene     0.084978065  0.1826037947 -2.097965e-02
Item_TypeHousehold              0.063450490  0.0055953571 -9.507522e-02
Item_TypeMeat                   0.058284576  0.0262373467 -8.317166e-02
Item_TypeOthers                -0.609182893 -0.4097269132 -1.101376e-02
Item_TypeSeafood               -0.166533749  0.3342656009  6.867412e-01
Item_TypeSnack Foods            0.024535181 -0.0013139336  4.651083e-03
Item_TypeSoft Drinks            0.260596285 -0.1164161778 -9.402753e-02
Item_TypeStarchy Foods         -0.086439375  0.1402710452  2.437700e-01
Item_MRP                        0.006042716  0.0557463610  1.028368e-01
Outlet_Establishment_Year1985  -0.025035454  0.0104604917 -8.696358e-03
Outlet_Establishment_Year1987  -0.018979129  0.0014555068  4.000645e-03
Outlet_Establishment_Year1997   0.022403576  0.0402894378  1.561269e-02
Outlet_Establishment_Year1998   0.062559136 -0.0388179567  1.351358e-02
Outlet_Establishment_Year1999   0.007439567 -0.0347503480  1.620270e-03
Outlet_Establishment_Year2002   0.086523102  0.0634222392 -1.077410e-01
Outlet_Establishment_Year2004   0.022797442 -0.0301680453 -1.149269e-02
Outlet_Establishment_Year2007  -0.126265604 -0.0320946756  1.045031e-01
Outlet_Establishment_Year2009  -0.013339817  0.0098909201 -6.542811e-03
Outlet_SizeOther                0.006896968  0.0004426694  5.060565e-03
Outlet_SizeHigh                -0.018979129  0.0014555068  4.000645e-03
Outlet_SizeMedium               0.003426339 -0.0063418927  1.236408e-03
Outlet_SizeSmall                0.002691572  0.0051730149 -9.147076e-03
Outlet_Location_TypeTier 1     -0.007970826  0.0019916938 -4.323875e-05
Outlet_Location_TypeTier 2     -0.011132654  0.0007912104 -9.889611e-03
Outlet_Location_TypeTier 3      0.018019319 -0.0025910601  9.536581e-03
Outlet_TypeGrocery Store        0.007660113 -0.0312574448 -6.179698e-03
Outlet_TypeSupermarket Type1   -0.003868712  0.0053667902  4.169597e-03
Outlet_TypeSupermarket Type2   -0.013339817  0.0098909201 -6.542811e-03
Outlet_TypeSupermarket Type3    0.011019871  0.0152854160  6.761785e-03
                                       PC25          PC26         PC27
Item_Weight                     0.165580973 -0.1898322405  0.333953649
Item_Fat_ContentLF             -0.396037169  0.1365902083  0.044825901
Item_Fat_Contentlow fat         0.546720124 -0.2774672890 -0.170055145
Item_Fat_ContentLow Fat         0.004885110 -0.0056896117  0.018551434
Item_Fat_Contentreg             0.215219710  0.6025127304 -0.040283813
Item_Fat_ContentRegular        -0.031408370 -0.1299806685  0.013697772
Item_Visibility                 0.026623110  0.0018170936  0.138949645
Item_TypeBaking Goods          -0.099981515 -0.0433271624 -0.254564646
Item_TypeBreads                -0.052259094  0.0422609124  0.196986341
Item_TypeBreakfast              0.139920524  0.1122631357 -0.043654644
Item_TypeCanned                -0.080258045  0.0295642840  0.049842048
Item_TypeDairy                 -0.090755469  0.2044340759  0.085689283
Item_TypeFrozen Foods           0.002622299 -0.0580823527 -0.056666211
Item_TypeFruits and Vegetables -0.012074055  0.0231784611  0.024609172
Item_TypeHard Drinks           -0.270855138 -0.0745632237  0.133981576
Item_TypeHealth and Hygiene    -0.067078883 -0.0118082157 -0.195983748
Item_TypeHousehold              0.071583414  0.0031850626  0.146626351
Item_TypeMeat                   0.144230791  0.1218385446  0.007369273
Item_TypeOthers                 0.003794341 -0.0003653837 -0.235283535
Item_TypeSeafood                0.479548969  0.2489649403  0.007789162
Item_TypeSnack Foods           -0.072444909  0.0129973178  0.108027670
Item_TypeSoft Drinks            0.230772873 -0.1196776735 -0.083276567
Item_TypeStarchy Foods          0.044208525 -0.5717457867  0.024094957
Item_MRP                       -0.126276960 -0.0047657645 -0.747520825
Outlet_Establishment_Year1985   0.034151252  0.0147462547  0.014446902
Outlet_Establishment_Year1987   0.010973387  0.0120116100  0.007453020
Outlet_Establishment_Year1997  -0.030572979 -0.0083428700  0.020288439
Outlet_Establishment_Year1998  -0.057366535 -0.0215069493 -0.045228534
Outlet_Establishment_Year1999  -0.015004644 -0.0097386217 -0.027602909
Outlet_Establishment_Year2002   0.024796859 -0.0005672401  0.017487744
Outlet_Establishment_Year2004  -0.020092941 -0.0017970826 -0.024053319
Outlet_Establishment_Year2007   0.008291610  0.0131923082  0.013247441
Outlet_Establishment_Year2009   0.025744381 -0.0055647036  0.011536362
Outlet_SizeOther               -0.008540825 -0.0030596050 -0.003524000
Outlet_SizeHigh                 0.010973387  0.0120116100  0.007453020
Outlet_SizeMedium              -0.013088389 -0.0058468774 -0.006163492
Outlet_SizeSmall                0.014618721  0.0008320775  0.004796529
Outlet_Location_TypeTier 1      0.018151334 -0.0046814355  0.002332182
Outlet_Location_TypeTier 2      0.008620520  0.0071814560  0.004419609
Outlet_Location_TypeTier 3     -0.024967486 -0.0025918574 -0.006388413
Outlet_TypeGrocery Store        0.024644198 -0.0053185154 -0.023514578
Outlet_TypeSupermarket Type1   -0.014177589  0.0031106127  0.004454170
Outlet_TypeSupermarket Type2    0.025744381 -0.0055647036  0.011536362
Outlet_TypeSupermarket Type3   -0.030347812  0.0064806660  0.006782679
                                       PC28         PC29         PC30
Item_Weight                    -0.559083091  0.385584663 -0.020625193
Item_Fat_ContentLF              0.004432372  0.104090796  0.234646910
Item_Fat_Contentlow fat         0.046421766 -0.022403796  0.163400770
Item_Fat_ContentLow Fat         0.036892255  0.041383515  0.249310043
Item_Fat_Contentreg            -0.085909252  0.032952319 -0.168760720
Item_Fat_ContentRegular        -0.030046459 -0.087140503 -0.349819062
Item_Visibility                 0.420919359  0.595886564 -0.075987628
Item_TypeBaking Goods          -0.169325802  0.133406170  0.159368583
Item_TypeBreads                -0.207868515  0.155495656  0.062078701
Item_TypeBreakfast             -0.116893169 -0.118418354  0.168627658
Item_TypeCanned                -0.093058771  0.069764107  0.138986329
Item_TypeDairy                  0.060992222 -0.209431968  0.050430499
Item_TypeFrozen Foods           0.065458669  0.070310235  0.155803313
Item_TypeFruits and Vegetables  0.091303679 -0.076560948  0.184750167
Item_TypeHard Drinks           -0.298348561  0.066087375 -0.259877030
Item_TypeHealth and Hygiene     0.083919301  0.080269314 -0.366161128
Item_TypeHousehold              0.150846229 -0.144738803 -0.420641069
Item_TypeMeat                   0.160724712  0.199704603  0.235225790
Item_TypeOthers                 0.089232274 -0.098381073 -0.222584602
Item_TypeSeafood               -0.096412616 -0.067004049  0.028170165
Item_TypeSnack Foods            0.068123363 -0.042260856  0.105046758
Item_TypeSoft Drinks           -0.248601117  0.089783659 -0.194909604
Item_TypeStarchy Foods          0.169046605 -0.139863601  0.063618241
Item_MRP                       -0.179920849  0.268975145 -0.003834353
Outlet_Establishment_Year1985   0.087943816  0.133961817 -0.060040250
Outlet_Establishment_Year1987   0.056453872  0.068133297 -0.022562829
Outlet_Establishment_Year1997  -0.106314172 -0.121988276  0.065115238
Outlet_Establishment_Year1998  -0.202265914 -0.276906274  0.082661860
Outlet_Establishment_Year1999  -0.007437559 -0.025004094  0.007320916
Outlet_Establishment_Year2002   0.069954896  0.101872646 -0.020037680
Outlet_Establishment_Year2004  -0.061076929 -0.094432963  0.019778849
Outlet_Establishment_Year2007   0.044555134  0.062858115 -0.020902903
Outlet_Establishment_Year2009   0.057687164  0.065842284 -0.021561021
Outlet_SizeOther               -0.031620004 -0.037779080  0.016978330
Outlet_SizeHigh                 0.056453872  0.068133297 -0.022562829
Outlet_SizeMedium              -0.013166146 -0.015382715  0.013593844
Outlet_SizeSmall                0.006238460  0.006615206 -0.015554378
Outlet_Location_TypeTier 1      0.043478264  0.054817069 -0.024203454
Outlet_Location_TypeTier 2      0.035441479  0.046619197 -0.014034615
Outlet_Location_TypeTier 3     -0.074010186 -0.095169486  0.035731205
Outlet_TypeGrocery Store        0.015270639  0.006382368 -0.039209084
Outlet_TypeSupermarket Type1   -0.002575650 -0.005679970  0.018841151
Outlet_TypeSupermarket Type2    0.057687164  0.065842284 -0.021561021
Outlet_TypeSupermarket Type3   -0.069851568 -0.063783661  0.034603047
                                       PC31          PC32          PC33
Item_Weight                     0.003782900  7.222783e-16  8.121949e-16
Item_Fat_ContentLF              0.052444993  2.554423e-01 -3.145113e-02
Item_Fat_Contentlow fat        -0.060013706  1.539536e-01 -1.895542e-02
Item_Fat_ContentLow Fat         0.009743282  6.630915e-01 -8.164262e-02
Item_Fat_Contentreg            -0.027592885  1.573058e-01 -1.936815e-02
Item_Fat_ContentRegular        -0.009809529  6.399401e-01 -7.879211e-02
Item_Visibility                -0.473351679 -4.128689e-16 -3.361839e-17
Item_TypeBaking Goods           0.023306447  5.174298e-02  1.640683e-01
Item_TypeBreads                 0.010736390  3.300508e-02  1.046536e-01
Item_TypeBreakfast              0.031270340  2.203487e-02  6.986888e-02
Item_TypeCanned                 0.024623120  5.177960e-02  1.641845e-01
Item_TypeDairy                  0.023327044  5.296836e-02  1.679538e-01
Item_TypeFrozen Foods           0.016821949  5.867975e-02  1.860637e-01
Item_TypeFruits and Vegetables  0.031088728  6.864953e-02  2.176762e-01
Item_TypeHard Drinks            0.022904567  3.054358e-02  9.684858e-02
Item_TypeHealth and Hygiene    -0.077380820  4.672687e-02  1.481631e-01
Item_TypeHousehold             -0.038024155  6.028889e-02  1.911660e-01
Item_TypeMeat                  -0.032379872  4.249345e-02  1.347396e-01
Item_TypeOthers                -0.080412853  2.721631e-02  8.629837e-02
Item_TypeSeafood               -0.001282428  1.685342e-02  5.343939e-02
Item_TypeSnack Foods            0.006881647  6.790063e-02  2.153015e-01
Item_TypeSoft Drinks           -0.016382033  4.342808e-02  1.377032e-01
Item_TypeStarchy Foods          0.045394829  2.550128e-02  8.086028e-02
Item_MRP                       -0.007289038  9.887924e-17  2.873136e-18
Outlet_Establishment_Year1985   0.299536025 -1.088914e-02  1.564942e-01
Outlet_Establishment_Year1987   0.113437032 -1.090504e-02  1.061161e-01
Outlet_Establishment_Year1997  -0.380538450 -1.667435e-02 -4.380147e-02
Outlet_Establishment_Year1998  -0.308093668 -1.179603e-02  6.888839e-03
Outlet_Establishment_Year1999  -0.012059302 -1.967556e-02  1.399674e-01
Outlet_Establishment_Year2002   0.091781938 -1.375746e-02 -3.461686e-02
Outlet_Establishment_Year2004  -0.134866992 -6.836474e-03  1.710935e-01
Outlet_Establishment_Year2007   0.098526999 -1.373794e-02 -3.456775e-02
Outlet_Establishment_Year2009   0.105489168 -2.845097e-03  2.193317e-01
Outlet_SizeOther               -0.037200896  2.197951e-03  2.797265e-01
Outlet_SizeHigh                 0.113437032 -1.016990e-02 -7.691858e-02
Outlet_SizeMedium              -0.124087654 -3.619947e-03 -2.948153e-01
Outlet_SizeSmall                0.088168572 -7.786363e-03 -1.737659e-02
Outlet_Location_TypeTier 1      0.173429350 -3.236948e-03  6.052637e-02
Outlet_Location_TypeTier 2      0.036702189 -1.818014e-02 -2.600601e-01
Outlet_Location_TypeTier 3     -0.194705089 -5.126284e-03 -6.738409e-02
Outlet_TypeGrocery Store        0.373140911  5.644600e-03  4.366526e-02
Outlet_TypeSupermarket Type1   -0.146747875  1.976078e-02  3.265389e-01
Outlet_TypeSupermarket Type2    0.105489168 -2.845097e-03  2.193317e-01
Outlet_TypeSupermarket Type3   -0.279479266  3.315836e-03  3.104108e-01
                                        PC34          PC35          PC36
Item_Weight                     0.000000e+00  0.000000e+00  0.000000e+00
Item_Fat_ContentLF              2.146054e-02 -9.368554e-03 -2.399021e-02
Item_Fat_Contentlow fat         1.293415e-02 -5.646374e-03 -1.445876e-02
Item_Fat_ContentLow Fat         5.570848e-02 -2.431942e-02 -6.227512e-02
Item_Fat_Contentreg             1.321577e-02 -5.769318e-03 -1.477358e-02
Item_Fat_ContentRegular         5.376345e-02 -2.347032e-02 -6.010082e-02
Item_Visibility                -3.400058e-16 -5.464379e-17 -3.148523e-16
Item_TypeBaking Goods          -1.317834e-01  7.365540e-02  1.380615e-01
Item_TypeBreads                -8.406016e-02  4.698226e-02  8.806471e-02
Item_TypeBreakfast             -5.612029e-02  3.136632e-02  5.879381e-02
Item_TypeCanned                -1.318767e-01  7.370753e-02  1.381592e-01
Item_TypeDairy                 -1.349044e-01  7.539972e-02  1.413311e-01
Item_TypeFrozen Foods          -1.494506e-01  8.352979e-02  1.565703e-01
Item_TypeFruits and Vegetables -1.748425e-01  9.772163e-02  1.831718e-01
Item_TypeHard Drinks           -7.779100e-02  4.347835e-02  8.149690e-02
Item_TypeHealth and Hygiene    -1.190080e-01  6.651504e-02  1.246774e-01
Item_TypeHousehold             -1.535489e-01  8.582038e-02  1.608638e-01
Item_TypeMeat                  -1.082260e-01  6.048883e-02  1.133817e-01
Item_TypeOthers                -6.931683e-02  3.874203e-02  7.261902e-02
Item_TypeSeafood               -4.292374e-02  2.399061e-02  4.496859e-02
Item_TypeSnack Foods           -1.729351e-01  9.665558e-02  1.811736e-01
Item_TypeSoft Drinks           -1.106063e-01  6.181925e-02  1.158755e-01
Item_TypeStarchy Foods         -6.494883e-02  3.630071e-02  6.804294e-02
Item_MRP                       -1.566672e-17  1.076079e-16 -7.462021e-17
Outlet_Establishment_Year1985   6.574752e-02  5.559729e-02 -1.786741e-03
Outlet_Establishment_Year1987  -1.981354e-01 -4.304042e-01 -1.196581e-02
Outlet_Establishment_Year1997  -1.892897e-02 -7.340154e-02  1.298653e-03
Outlet_Establishment_Year1998  -1.720685e-01  1.567135e-02  1.258160e-01
Outlet_Establishment_Year1999   1.703659e-01  1.228729e-01 -1.337255e-01
Outlet_Establishment_Year2002  -2.222885e-01  2.457002e-01 -1.583292e-01
Outlet_Establishment_Year2004  -7.575560e-02  3.917044e-02 -2.946381e-01
Outlet_Establishment_Year2007  -2.219731e-01  2.453516e-01 -1.581046e-01
Outlet_Establishment_Year2009   1.236538e-01 -5.682380e-02 -1.736413e-02
Outlet_SizeOther                2.200628e-01  8.261958e-02 -3.090181e-01
Outlet_SizeHigh                 5.963437e-02  3.879955e-01 -4.026207e-02
Outlet_SizeMedium              -2.763646e-01  1.017246e-01  8.628631e-02
Outlet_SizeSmall                8.236095e-03  3.800373e-01 -1.119288e-01
Outlet_Location_TypeTier 1      1.744123e-01  9.761055e-02  1.686346e-01
Outlet_Location_TypeTier 2      2.676431e-01 -6.740119e-02  6.213108e-01
Outlet_Location_TypeTier 3      3.857601e-01  4.708753e-01  1.454484e-01
Outlet_TypeGrocery Store        1.378438e-01 -1.315882e-01  8.279595e-02
Outlet_TypeSupermarket Type1    3.086310e-01 -5.819239e-03  1.139949e-01
Outlet_TypeSupermarket Type2    1.236538e-01 -5.682380e-02 -1.736413e-02
Outlet_TypeSupermarket Type3    1.936331e-01 -1.601014e-01 -3.336210e-02
                                        PC37          PC38          PC39
Item_Weight                     1.711910e-15  0.000000e+00  0.000000e+00
Item_Fat_ContentLF             -6.466572e-03  9.002479e-03  4.280543e-04
Item_Fat_Contentlow fat        -3.897366e-03  5.425742e-03  2.579859e-04
Item_Fat_ContentLow Fat        -1.678629e-02  2.336914e-02  1.111167e-03
Item_Fat_Contentreg            -3.982227e-03  5.543882e-03  2.636032e-04
Item_Fat_ContentRegular        -1.620021e-02  2.255322e-02  1.072372e-03
Item_Visibility                -2.596142e-16 -1.435484e-16  3.794437e-16
Item_TypeBaking Goods           2.907487e-02 -5.730185e-02 -2.041462e-02
Item_TypeBreads                 1.854587e-02 -3.655089e-02 -1.302179e-02
Item_TypeBreakfast              1.238160e-02 -2.440212e-02 -8.693615e-03
Item_TypeCanned                 2.909545e-02 -5.734240e-02 -2.042907e-02
Item_TypeDairy                  2.976343e-02 -5.865888e-02 -2.089809e-02
Item_TypeFrozen Foods           3.297271e-02 -6.498385e-02 -2.315145e-02
Item_TypeFruits and Vegetables  3.857482e-02 -7.602471e-02 -2.708492e-02
Item_TypeHard Drinks            1.716273e-02 -3.382495e-02 -1.205063e-02
Item_TypeHealth and Hygiene     2.625627e-02 -5.174685e-02 -1.843557e-02
Item_TypeHousehold              3.387690e-02 -6.676586e-02 -2.378632e-02
Item_TypeMeat                   2.387747e-02 -4.705862e-02 -1.676532e-02
Item_TypeOthers                 1.529310e-02 -3.014022e-02 -1.073790e-02
Item_TypeSeafood                9.470098e-03 -1.866403e-02 -6.649333e-03
Item_TypeSnack Foods            3.815401e-02 -7.519535e-02 -2.678945e-02
Item_TypeSoft Drinks            2.440265e-02 -4.809365e-02 -1.713407e-02
Item_TypeStarchy Foods          1.432941e-02 -2.824094e-02 -1.006125e-02
Item_MRP                       -1.203464e-16  1.299687e-16 -2.458157e-16
Outlet_Establishment_Year1985  -1.053760e-01 -1.308960e-01  1.279234e-01
Outlet_Establishment_Year1987  -1.931242e-01  4.875457e-02 -2.980661e-01
Outlet_Establishment_Year1997   3.508235e-03 -2.896492e-01  2.448706e-02
Outlet_Establishment_Year1998  -1.604897e-01  3.124222e-01  1.767141e-01
Outlet_Establishment_Year1999   3.640055e-01 -1.199106e-01 -1.225008e-01
Outlet_Establishment_Year2002  -4.284348e-02  3.198518e-01 -9.963641e-02
Outlet_Establishment_Year2004  -3.410668e-01 -9.558302e-02 -1.675219e-01
Outlet_Establishment_Year2007  -4.278270e-02  3.193980e-01 -9.949505e-02
Outlet_Establishment_Year2009  -5.181972e-02  8.761677e-02  9.288261e-02
Outlet_SizeOther               -3.103992e-01 -4.757003e-01 -9.599298e-03
Outlet_SizeHigh                -1.342476e-01 -1.648311e-01  4.335671e-01
Outlet_SizeMedium              -4.172978e-01 -1.256952e-01  3.134040e-01
Outlet_SizeSmall                1.199903e-01  1.242251e-01  8.814034e-02
Outlet_Location_TypeTier 1     -5.804555e-01  1.745579e-01 -3.196156e-01
Outlet_Location_TypeTier 2     -8.787355e-02 -1.096317e-01 -4.496187e-02
Outlet_Location_TypeTier 3      1.705637e-02  5.287406e-02 -4.254619e-01
Outlet_TypeGrocery Store        3.927907e-02  2.825815e-02  1.895652e-01
Outlet_TypeSupermarket Type1   -8.216510e-02  3.170777e-01  3.946802e-01
Outlet_TypeSupermarket Type2   -5.181972e-02  8.761677e-02  9.288261e-02
Outlet_TypeSupermarket Type3   -1.664695e-02  2.842976e-01  8.035688e-02
                                        PC40          PC41          PC42
Item_Weight                     0.000000e+00  0.000000e+00  0.000000e+00
Item_Fat_ContentLF              6.978128e-03  5.525517e-03 -1.071040e-02
Item_Fat_Contentlow fat         4.205678e-03  3.330197e-03 -6.455095e-03
Item_Fat_ContentLow Fat         1.811422e-02  1.434345e-02 -2.780265e-02
Item_Fat_Contentreg             4.297252e-03  3.402709e-03 -6.595647e-03
Item_Fat_ContentRegular         1.748177e-02  1.384265e-02 -2.683194e-02
Item_Visibility                 1.968911e-16 -2.463307e-16 -2.064321e-16
Item_TypeBaking Goods          -3.017055e-02 -8.738842e-03  1.116231e-02
Item_TypeBreads                -1.924476e-02 -5.574209e-03  7.120055e-03
Item_TypeBreakfast             -1.284820e-02 -3.721456e-03  4.753495e-03
Item_TypeCanned                -3.019190e-02 -8.745027e-03  1.117021e-02
Item_TypeDairy                 -3.088505e-02 -8.945796e-03  1.142666e-02
Item_TypeFrozen Foods          -3.421528e-02 -9.910389e-03  1.265875e-02
Item_TypeFruits and Vegetables -4.002850e-02 -1.159418e-02  1.480949e-02
Item_TypeHard Drinks           -1.780950e-02 -5.158487e-03  6.589046e-03
Item_TypeHealth and Hygiene    -2.724573e-02 -7.891675e-03  1.008020e-02
Item_TypeHousehold             -3.515354e-02 -1.018215e-02  1.300588e-02
Item_TypeMeat                  -2.477729e-02 -7.176694e-03  9.166944e-03
Item_TypeOthers                -1.586942e-02 -4.596547e-03  5.871267e-03
Item_TypeSeafood               -9.826977e-03 -2.846365e-03  3.635723e-03
Item_TypeSnack Foods           -3.959183e-02 -1.146770e-02  1.464793e-02
Item_TypeSoft Drinks           -2.532225e-02 -7.334542e-03  9.368567e-03
Item_TypeStarchy Foods         -1.486941e-02 -4.306896e-03  5.501290e-03
Item_MRP                       -1.348476e-17 -6.976163e-17 -2.708744e-16
Outlet_Establishment_Year1985   5.475257e-01  3.179588e-01  2.452401e-02
Outlet_Establishment_Year1987   1.428094e-01  1.091229e-01 -4.288337e-01
Outlet_Establishment_Year1997  -1.031975e-01  4.341582e-01 -1.730626e-01
Outlet_Establishment_Year1998   3.438212e-01 -5.568666e-02 -1.559962e-01
Outlet_Establishment_Year1999   4.173728e-02  1.671687e-01 -3.880572e-01
Outlet_Establishment_Year2002  -9.766577e-02  2.259549e-01 -1.142343e-01
Outlet_Establishment_Year2004  -1.594415e-01  2.623578e-01  1.084801e-01
Outlet_Establishment_Year2007  -9.752721e-02  2.256343e-01 -1.140722e-01
Outlet_Establishment_Year2009   3.544268e-02  2.424384e-02 -1.247588e-01
Outlet_SizeOther                6.350924e-02 -1.840657e-01 -1.588649e-01
Outlet_SizeHigh                -2.203592e-01 -1.356946e-01 -7.208966e-02
Outlet_SizeMedium              -5.906734e-02  1.554510e-01 -1.772818e-01
Outlet_SizeSmall                1.522464e-01 -2.358334e-01 -4.792928e-01
Outlet_Location_TypeTier 1     -8.741537e-02 -2.066884e-01  7.981696e-02
Outlet_Location_TypeTier 2     -6.687015e-03  4.257934e-02 -3.401735e-01
Outlet_Location_TypeTier 3      3.021721e-02  2.404326e-01  7.840256e-02
Outlet_TypeGrocery Store       -4.771891e-01  3.734376e-01 -6.452197e-02
Outlet_TypeSupermarket Type1    1.665576e-01  2.720295e-01  2.027760e-01
Outlet_TypeSupermarket Type2    3.544268e-02  2.424384e-02 -1.247588e-01
Outlet_TypeSupermarket Type3   -3.826649e-01 -2.148736e-01 -2.706667e-01
                                        PC43          PC44
Item_Weight                     0.000000e+00  0.000000e+00
Item_Fat_ContentLF             -9.125298e-03  4.440892e-16
Item_Fat_Contentlow fat        -5.499765e-03  2.296005e-16
Item_Fat_ContentLow Fat        -2.368796e-02  1.090912e-15
Item_Fat_Contentreg            -5.619516e-03  1.473807e-16
Item_Fat_ContentRegular        -2.286091e-02  1.091790e-15
Item_Visibility                 2.242130e-16 -1.864678e-17
Item_TypeBaking Goods           1.489422e-02 -1.203735e-15
Item_TypeBreads                 9.500513e-03 -7.405873e-16
Item_TypeBreakfast              6.342737e-03 -5.864615e-16
Item_TypeCanned                 1.490476e-02 -1.034930e-15
Item_TypeDairy                  1.524695e-02 -1.093600e-15
Item_TypeFrozen Foods           1.689097e-02 -1.266042e-15
Item_TypeFruits and Vegetables  1.976077e-02 -1.435362e-15
Item_TypeHard Drinks            8.791970e-03 -5.904748e-16
Item_TypeHealth and Hygiene     1.345033e-02 -9.836155e-16
Item_TypeHousehold              1.735416e-02 -1.259196e-15
Item_TypeMeat                   1.223174e-02 -7.980565e-16
Item_TypeOthers                 7.834215e-03 -6.162686e-16
Item_TypeSeafood                4.851259e-03 -3.773636e-16
Item_TypeSnack Foods            1.954520e-02 -1.445165e-15
Item_TypeSoft Drinks            1.250077e-02 -7.163654e-16
Item_TypeStarchy Foods          7.340543e-03 -5.108468e-16
Item_MRP                        4.920923e-17 -2.673901e-17
Outlet_Establishment_Year1985  -2.079682e-01  7.700781e-15
Outlet_Establishment_Year1987   1.859461e-01 -3.509235e-15
Outlet_Establishment_Year1997  -1.883571e-01 -6.034224e-15
Outlet_Establishment_Year1998  -2.629365e-01  1.952004e-14
Outlet_Establishment_Year1999  -2.004984e-01  7.018348e-15
Outlet_Establishment_Year2002  -8.713004e-02 -4.449455e-15
Outlet_Establishment_Year2004  -1.756842e-01 -5.538360e-15
Outlet_Establishment_Year2007  -8.700642e-02 -4.477885e-15
Outlet_Establishment_Year2009  -1.250660e-01 -7.071068e-01
Outlet_SizeOther                2.163255e-01  2.775558e-17
Outlet_SizeHigh                -3.850250e-01  1.592476e-14
Outlet_SizeMedium               3.769973e-01 -1.786071e-14
Outlet_SizeSmall                3.432151e-01  1.582068e-15
Outlet_Location_TypeTier 1     -1.497236e-01  9.103829e-15
Outlet_Location_TypeTier 2     -1.754363e-01  8.409939e-15
Outlet_Location_TypeTier 3      2.269439e-01 -1.729172e-14
Outlet_TypeGrocery Store        1.949954e-01 -2.602779e-14
Outlet_TypeSupermarket Type1    3.034611e-01 -1.823541e-14
Outlet_TypeSupermarket Type2   -1.250660e-01  7.071068e-01
Outlet_TypeSupermarket Type3   -7.859574e-02  5.884182e-15
```

========================================================

We got 44 principa components loadings. The question is, if we got correct result or not. As per fact, In a data set, the maximum no of principal component is a minimum of (n-1,p). To verify this lets look at the first 4 principal components and first 5 rows.


```r
prin_comp$rotation[1:5,1:4]
```

```
                                 PC1           PC2          PC3
Item_Weight              0.001044124 -0.0004384151 -0.021454235
Item_Fat_ContentLF      -0.009172576  0.0019042858  0.008383420
Item_Fat_Contentlow fat -0.003428769 -0.0055492764 -0.003302283
Item_Fat_ContentLow Fat  0.007836003 -0.0065124968 -0.021090747
Item_Fat_Contentreg     -0.001251767 -0.0033104683 -0.006895653
                                  PC4
Item_Weight              0.0009241751
Item_Fat_ContentLF       0.0067791081
Item_Fat_Contentlow fat  0.0200639450
Item_Fat_ContentLow Fat -0.0263226605
Item_Fat_Contentreg     -0.0128246294
```

## So, We are absolutely right here.

========================================================

## Step 3.) In order to compute the principal component score vector, we don’t need to multiply the loading with data. Rather, the matrix x has the principal component score vectors in a 8523 × 44 dimension.


```r
dim(prin_comp$x)
```

```
[1] 8523   44
```


========================================================
Let’s plot the resultant principal components.


```r
biplot(prin_comp, scale = 0)
```

![plot of chunk unnamed-chunk-27](Assignment3_Abhimanyu_Kumar-figure/unnamed-chunk-27-1.png)

To ensure that arrows are scaled to represent the loadings I gave value of parameter scale =0.
After focusing on the extreme ends of the graph we can infer that first PC corresponds to a measure of Outlet_TypeSupermarket, Outlet_Establishment_Year 2007. Similarly, it can be said that the second component corresponds to a measure of Outlet_Location_TypeTier1, Outlet_Sizeother. For exact measure of a variable in a component, we need to look at rotation matrix(above) again.

========================================================

## Step 4.) From the prcomp() function we can compute the standard deviation of each principal component. Here, sdev refers to the standard deviation of principal components.

## standard deviation

```r
#compute standard deviation of each principal component
std_dev <- prin_comp$sdev
```

## Variance

```r
#compute variance
pr_var <- std_dev^2
```


```r
#check variance of first 10 components
pr_var[1:10]
```

```
 [1] 4.564631 3.219279 2.744497 2.535883 2.190449 2.021640 1.935010
 [8] 1.266311 1.205757 1.169495
```

Here my main aim is to find the components which expains the maximum variance because I want to retain as much information as possible by using these components. We can infer that the higher is the explained variance,higher will be the information containe din those components.

========================================================

To compute the proportion of variance explained by each component, I simply divide the variance by sum of total variance. 

```r
#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]
```

```
 [1] 0.10374160 0.07316543 0.06237493 0.05763369 0.04978294 0.04594636
 [7] 0.04397749 0.02877979 0.02740357 0.02657943 0.02631063 0.02577906
[13] 0.02549683 0.02511309 0.02477200 0.02469167 0.02460995 0.02446028
[19] 0.02398721 0.02373467
```

This shows that first principal component explains 10.3% variance. Second component explains 7.3% variance. Third component explains 6.2% variance and so on. So, how do we decide how many components should I select for modeling stage ?

========================================================

The answer to this question is provided by a scree plot. A scree plot is used to access components or factors which explains the most of variability in the data. It represents values in descending order.


```r
#scree plot
plot(prop_varex, xlab = "Principal Component",
             ylab = "Proportion of Variance Explained",
             type = "b")
```

![plot of chunk unnamed-chunk-32](Assignment3_Abhimanyu_Kumar-figure/unnamed-chunk-32-1.png)

The plot above shows that ~ 30 components explains around 98.4% variance in the data set. In order words, using PCA we have reduced 44 predictors to 30 without compromising on explained variance. This is the power of PCA> Let’s do a confirmation check, by plotting a cumulative variance plot. This will give us a clear picture of number of components.

========================================================


```r
#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
              ylab = "Cumulative Proportion of Variance Explained",
              type = "b")
```

![plot of chunk unnamed-chunk-33](Assignment3_Abhimanyu_Kumar-figure/unnamed-chunk-33-1.png)

This plot shows that 30 components results in variance close to ~ 98%. Therefore, in this case, we’ll select number of components as 30 [PC1 to PC30] and proceed to the modeling stage. This completes the steps to implement PCA on train data. For modeling, I will use these 30 components as predictor variables and follow the normal procedures.

========================================================

## Predictive Modeling with PCA Components

As I have calculated the PC on the training sets, so we can now get on the process of predicting on test data using these components. 

So, what should I do?

I will do exactly the same transformation to the test set as what I did to training set, including the center and scaling feature.


```r
#add a training set with principal components
train.data <- data.frame(Item_Outlet_Sales = train$Item_Outlet_Sales, prin_comp$x)
```


```r
#I am interested in first 30 PCAs
train.data <- train.data[,1:31]
```

========================================================


```r
#run a decision tree
install.packages("rpart", repos = "http://cran.us.r-project.org")
```

```

The downloaded binary packages are in
	/var/folders/mn/7jk0pyy53cjbzyt8ryfh3wzc0000gn/T//RtmpbK35S5/downloaded_packages
```

```r
library(rpart)
rpart.model <- rpart(Item_Outlet_Sales ~ .,data = train.data, method = "anova")
rpart.model
```

```
n= 8523 

node), split, n, deviance, yval
      * denotes terminal node

 1) root 8523 24817270000 2181.2890  
   2) PC3>=1.671844 1083    73623120  339.8285 *
   3) PC3< 1.671844 7440 20536640000 2449.3400  
     6) PC27>=-0.02464682 3838  5499852000 1780.8280  
      12) PC27>=1.079021 1017   586793000 1132.8500 *
      13) PC27< 1.079021 2821  4332103000 2014.4310  
        26) PC9>=-0.3193864 1627  1972546000 1675.5750  
          52) PC19>=-1.337507 1439  1294309000 1534.0690 *
          53) PC19< -1.337507 188   428870300 2758.6960 *
        27) PC9< -0.3193864 1194  1918174000 2476.1710 *
     7) PC27< -0.02464682 3602 11493930000 3161.6530  
      14) PC6>=-2.648969 3155  8039466000 2925.5700  
        28) PC9>=0.1215489 1412  2827138000 2417.5500  
          56) PC27>=-1.461566 1182  1950290000 2217.1510 *
          57) PC27< -1.461566 230   585430300 3447.4280 *
        29) PC9< 0.1215489 1743  4552703000 3337.1150 *
      15) PC6< -2.648969 447  2037483000 4827.9660  
        30) PC8< -0.7209545 118   302595100 3358.4530 *
        31) PC8>=-0.7209545 329  1388678000 5355.0250 *
```

========================================================

```r
#transform test into PCA
test.data <- predict(prin_comp, newdata = pca.test)
test.data <- as.data.frame(test.data)
```


```r
#select the first 30 components
test.data <- test.data[,1:30]

#make prediction on test data
rpart.prediction <- predict(rpart.model, test.data)

#For fun, finally check your score of leaderboard
sample <- read.csv("sample.csv")
final.sub <- data.frame(Item_Identifier = sample$Item_Identifier, Outlet_Identifier = sample$Outlet_Identifier, Item_Outlet_Sales = rpart.prediction)

head(final.sub)
```

```
     Item_Identifier Outlet_Identifier Item_Outlet_Sales
8524           FDW58            OUT049         1132.8499
8525           FDW14            OUT017         1534.0694
8526           NCN55            OUT010          339.8285
8527           FDQ58            OUT017         3337.1153
8528           FDY38            OUT027         5355.0254
8529           FDH56            OUT046         2476.1710
```

```r
# To get the whole predicted data.
#write.csv(final.sub, "pca.csv",row.names = F)
```
========================================================

