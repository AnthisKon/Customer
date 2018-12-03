# Data Analyst @ Gousto
Konstantinos Anthis  
01 December 2018  



<script>
$(document).ready(function() {
$head = $('#header');
$head.prepend('<img src=\"image.jpg\" style=\"float: right;width: 200px;\"/>')
});
</script>






# Approach and Actions {.tabset .tabset-fade .tabset-pills}

## Approach

The data set of this exercise contains information about **852,761 orders** made by **156,189 customers** from *January 2014 to January 2016*. There are only six features for each order in the initial data set: Order ID, User ID , Period ID, Payment Date, Due Ammount and Paid Ammmount. 

I included findings that are important and support my proposals.

**I am assuming that the orders are about gousto's product.**


My approach is to analyse this dataset looking at two sides:


**1. By customer**

  - Create a data set aggregating data by customer and engineering new features
  - New features: Tenure, Frequency of purchase, Monetary value, recency of purchase, Churn and Retention,
    cost of the customer and net value of the customer.
    
**2. By order**

  - Create some descriptive measures to find out information for orders per year, month, week day or period
  
  
  

## Actions



Proposed actions: 

1. **Create a relationship with new customers to retain new customers for more than a month: **

    * Analyze what channels of communication with customer are available and how the customer responds to each channel
    * Make special offers for new customers (what about cooking tools or wine to company the meal?). (As I was writing this I saw your "Get 35% off all boxes in your first month" offer in gousto website)
    * Use the customer base to create a community where the customers can help each other, welcome new customers, share experiences and cooking 'secrets', opinions and recomendations about recipes etc
    * 'Personal chef' option for new customers: A chef/cook goes to the customer's location in order to cook with the customer (like a live course). This service can be given for free during the first week or charging a small ammount

2. **Special offers and activities during the weekend:**

    * Saturday and Sunday have the fewest orders with the highest avegare ammount paid and costs at the same level as the rest of the days
    * Weekend community cooking competitions: Each customer can cook and share the dish online in order to get votes about the appearence of the dish, the creativity etc. Highest voted customers get a gift from gousto. 
    * Even on site competitions: Create a space for cooking  courses and competitions for the weekend


*Note: Some of these actions may be already in place*





# Orders Analysis {.tabset .tabset-fade .tabset-pills}

## Week Days orders

* **During the Weekend (Saturday and Sunday) customers seem to spend more on average than during other days:**


```r
gousto  %>% mutate(payment_made = factor(weekdays(payment_made), 
                                         c('Monday',
                                           'Tuesday',
                                           'Wednesday',
                                           'Thursday',
                                           'Friday',
                                           'Saturday',
                                           'Sunday'))) %>% 
  ggplot(aes(x = payment_made, 
             y = total_paid,
             fill = payment_made)) +
  geom_boxplot() +
  theme_minimal() +
  scale_fill_brewer(palette = 'Dark2', 
                    name = 'Weekday') +
  ggtitle('Payments per Weekday') +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = 'none')
```

<div class="figure" style="text-align: center">
<img src="Report_files/figure-html/unnamed-chunk-1-1.png" alt="Weekend seems to have higher payments compared to other weekdays. Increasing the ammount of orders during the weekend is the way to go"  />
<p class="caption">Weekend seems to have higher payments compared to other weekdays. Increasing the ammount of orders during the weekend is the way to go</p>
</div>

* At the same time, during the weekend the ammount of orders reduces significantly:


```r
gousto  %>% group_by(Weekday = weekdays(payment_made)) %>% 
  summarise(Orders = format(n(), 
                            big.mark = ',',
                            decimal.mark = '.'), 
            'Avg Paid' = paste(format(mean(total_paid), 
                                      big.mark = ',',
                                      decimal.mark = '.', 
                                      digits = 3),
                               '£'),
            'Avg Due' = paste(format(mean(total_due), 
                                     big.mark = ',',
                                     decimal.mark = '.', 
                                     digits = 3),
                              "£"),
            'TTL Paid' = paste(format(sum(total_paid), 
                                     big.mark = ',',
                                     decimal.mark = '.'),
                              "£"),
            'TTL Due' = paste(format(sum(total_due), 
                                     big.mark = ',',
                                     decimal.mark = '.'),
                              "£")) 
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Weekday"],"name":[1],"type":["chr"],"align":["left"]},{"label":["Orders"],"name":[2],"type":["chr"],"align":["left"]},{"label":["Avg Paid"],"name":[3],"type":["chr"],"align":["left"]},{"label":["Avg Due"],"name":[4],"type":["chr"],"align":["left"]},{"label":["TTL Paid"],"name":[5],"type":["chr"],"align":["left"]},{"label":["TTL Due"],"name":[6],"type":["chr"],"align":["left"]}],"data":[{"1":"Friday","2":"108,683","3":"36.4 £","4":"35 £","5":"3,956,705 £","6":"3,806,220 £"},{"1":"Monday","2":"170,246","3":"37.1 £","4":"35 £","5":"6,321,033 £","6":"5,953,422 £"},{"1":"Saturday","2":"82,718","3":"39.6 £","4":"35.1 £","5":"3,273,461 £","6":"2,903,679 £"},{"1":"Sunday","2":"82,487","3":"39.4 £","4":"35 £","5":"3,253,644 £","6":"2,888,778 £"},{"1":"Thursday","2":"109,933","3":"36.4 £","4":"35 £","5":"4,005,769 £","6":"3,847,560 £"},{"1":"Tuesday","2":"167,328","3":"37.1 £","4":"34.9 £","5":"6,210,201 £","6":"5,848,033 £"},{"1":"Wednesday","2":"131,366","3":"37.3 £","4":"35 £","5":"4,906,387 £","6":"4,604,180 £"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


## Orders by month

The number of Orders increase during December 


```r
gousto  %>% 
  group_by(Month = month.abb[month(payment_made)]) %>% 
  summarise(Orders = n(), 
            Avg_Paid = mean(total_paid),
            
            Avg_Due = mean(total_due),
            Paid = sum(total_paid),
            Due = sum(total_due))  %>% 
  ggplot(aes(x = Month, 
             y = Orders)) + 
  geom_bar(fill = "#CD3700", 
           stat = "identity") +
  theme_minimal() +
  ggtitle('Number of orders per month')+
  theme(plot.title = element_text(hjust = 0.5))
```

![](Report_files/figure-html/unnamed-chunk-3-1.png)<!-- -->




# Customers Analysis {.tabset .tabset-fade .tabset-pills}

The orders data set is aggregated by customers in orders to analyze customer behaviours, detect problems and propose actions:



**The most important problem discovered by analysing customer retention and churn.**


```r
today = dmy('01-01-2017')

Customer <- gousto %>% 
  group_by(user_id) %>% 
  summarise(Frequency = n(), 
            Last_pay = max(payment_made),
            First_pay = min(payment_made),
            Recency = difftime(today , 
                               max(payment_made) ,
                               units = c("days")),
            Paid = sum(total_paid),
            Due = sum(total_due),
            Max_Paid = max(total_paid),
            Max_Due = max(total_due),
            Avg_Paid = mean(total_paid),
            Avg_due = mean(total_due),
            Purchased_Again = ifelse((year(First_pay) < year(Last_pay)),
                                     'YES',
                                     "NO"),
            Tenure = difftime(Last_pay, 
                              First_pay,
                              c("days")),
            Diff = Paid - Due,
            Same_month_churn = case_when(Tenure == 0 ~ '0',
                                         Tenure <= 30 ~ '1',
                                         Tenure > 30 ~ '2')
            )
```



## **The Problem: Churn, Retention**
* **Major Problem:** **~50%** of the customers churn **at the first day of their order** and **~80%** of the customers churned **the same month they purchased for the first time**: 

```r
Customer %>% 
  group_by(Retention = factor(Same_month_churn, 
                              labels = c('Same day churn',
                                         'Same month churn',
                                         'Retained more than 1 month'))) %>% 
  summarise(Count = n(), 
            'Average net Value' = format(mean(Diff),
                                         decimal.mark = '.',
                                         digits = 3)) %>% 
  mutate('Base Percentage' = paste(format((Count/sum(Count))*100,
                                          decimal.mark = '.',
                                          digits = 4),
                                   '%'),
         Count = format(Count, 
                        big.mark = ',')) %>% 
  kable('html', align = 'rccc') %>% 
  kable_styling(bootstrap_options = "striped")
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
<thead><tr>
<th style="text-align:right;"> Retention </th>
   <th style="text-align:center;"> Count </th>
   <th style="text-align:center;"> Average net Value </th>
   <th style="text-align:center;"> Base Percentage </th>
  </tr></thead>
<tbody>
<tr>
<td style="text-align:right;"> Same day churn </td>
   <td style="text-align:center;"> 80,515 </td>
   <td style="text-align:center;"> 2.92 </td>
   <td style="text-align:center;"> 51.55 % </td>
  </tr>
<tr>
<td style="text-align:right;"> Same month churn </td>
   <td style="text-align:center;"> 47,420 </td>
   <td style="text-align:center;"> 5.6 </td>
   <td style="text-align:center;"> 30.36 % </td>
  </tr>
<tr>
<td style="text-align:right;"> Retained more than 1 month </td>
   <td style="text-align:center;"> 28,254 </td>
   <td style="text-align:center;"> 55.7 </td>
   <td style="text-align:center;"> 18.09 % </td>
  </tr>
</tbody>
</table>



## New Customers (Acquisition) per year

**How many customers are acquired each year ??**

```r
Customer %>%
  group_by('New Customers' = year(First_pay)) %>% 
  summarise( Count = format(n(),
                            big.mark = ',') ) %>% 
  kable('html', align = 'cc') %>% 
  kable_styling(bootstrap_options = "striped")
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
<thead><tr>
<th style="text-align:center;"> New Customers </th>
   <th style="text-align:center;"> Count </th>
  </tr></thead>
<tbody>
<tr>
<td style="text-align:center;"> 2014 </td>
   <td style="text-align:center;"> 75,906 </td>
  </tr>
<tr>
<td style="text-align:center;"> 2015 </td>
   <td style="text-align:center;"> 79,207 </td>
  </tr>
<tr>
<td style="text-align:center;"> 2016 </td>
   <td style="text-align:center;"> 1,076 </td>
  </tr>
</tbody>
</table>



## Yearly Retention

The table bellow shows the first year a customer made the first purchase and the last. We can see how many customers churned the same year they purchased for the first time and 

```r
Customer %>%
  group_by('First Purchase' = year(First_pay), 
           'Last Purchase'= year(Last_pay)) %>% 
  summarise( Customers = format(n(), 
                                big.mark = ',')) %>% 
  kable('html', align = 'ccc') %>% 
  kable_styling(bootstrap_options = "striped")
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
<thead><tr>
<th style="text-align:center;"> First Purchase </th>
   <th style="text-align:center;"> Last Purchase </th>
   <th style="text-align:center;"> Customers </th>
  </tr></thead>
<tbody>
<tr>
<td style="text-align:center;"> 2014 </td>
   <td style="text-align:center;"> 2014 </td>
   <td style="text-align:center;"> 67,955 </td>
  </tr>
<tr>
<td style="text-align:center;"> 2014 </td>
   <td style="text-align:center;"> 2015 </td>
   <td style="text-align:center;"> 5,848 </td>
  </tr>
<tr>
<td style="text-align:center;"> 2014 </td>
   <td style="text-align:center;"> 2016 </td>
   <td style="text-align:center;"> 2,103 </td>
  </tr>
<tr>
<td style="text-align:center;"> 2015 </td>
   <td style="text-align:center;"> 2015 </td>
   <td style="text-align:center;"> 74,732 </td>
  </tr>
<tr>
<td style="text-align:center;"> 2015 </td>
   <td style="text-align:center;"> 2016 </td>
   <td style="text-align:center;"> 4,475 </td>
  </tr>
<tr>
<td style="text-align:center;"> 2016 </td>
   <td style="text-align:center;"> 2016 </td>
   <td style="text-align:center;"> 1,076 </td>
  </tr>
</tbody>
</table>


## Sample Customer Data

Just a sample o the customer data:


```r
datatable(head(Customer,200))
```

<!--html_preserve--><div id="htmlwidget-3c2e6797848f861f943f" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-3c2e6797848f861f943f">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187","188","189","190","191","192","193","194","195","196","197","198","199","200"],[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200],[2,1,9,1,3,1,3,81,1,1,2,12,2,5,35,9,3,2,1,1,1,59,2,1,1,2,1,3,1,2,1,1,1,2,2,1,1,1,52,1,1,1,1,1,1,1,1,1,40,1,1,1,26,2,1,5,1,2,1,1,1,1,1,1,2,2,9,6,3,14,1,1,1,1,1,1,4,4,6,1,6,2,2,1,67,1,1,1,1,22,1,1,9,68,1,1,2,1,1,18,1,1,1,1,1,4,2,4,1,1,1,1,34,1,1,3,1,8,1,1,1,12,1,2,1,1,1,6,3,1,2,4,5,8,2,5,7,6,15,1,4,1,7,1,1,1,1,70,55,1,5,10,1,2,3,4,1,2,3,5,2,88,1,1,1,1,1,5,4,29,3,1,1,3,4,2,41,1,1,1,4,1,6,78,17,6,1,2,2,4,1,11,23,3,3,1,1,2,5,5],["2014-03-26","2015-06-01","2015-09-18","2015-10-05","2015-12-28","2015-02-19","2015-08-25","2015-12-29","2014-08-08","2014-05-21","2014-07-14","2014-08-27","2014-05-02","2015-10-01","2015-08-05","2015-10-05","2014-12-17","2015-12-29","2014-11-03","2015-10-21","2015-08-14","2015-12-31","2015-05-28","2014-05-01","2015-07-06","2014-11-24","2014-07-11","2015-09-16","2015-12-31","2015-04-22","2015-10-08","2014-04-11","2014-08-22","2015-11-13","2015-07-10","2015-11-19","2014-08-04","2014-12-02","2015-12-30","2015-01-09","2014-04-08","2015-02-23","2015-04-20","2014-10-06","2015-07-15","2015-11-16","2014-04-30","2015-11-30","2015-12-31","2014-04-28","2014-11-17","2015-11-30","2014-12-25","2015-10-21","2014-01-06","2014-03-19","2014-07-08","2015-03-25","2015-06-19","2015-09-15","2015-09-29","2016-01-01","2015-11-13","2014-11-07","2015-03-09","2014-10-31","2014-06-03","2015-12-30","2015-12-30","2015-03-04","2015-11-06","2014-09-12","2015-12-16","2015-06-11","2014-04-24","2015-08-12","2015-06-05","2015-12-31","2015-09-02","2014-12-03","2014-10-14","2015-05-07","2014-02-17","2014-01-28","2015-12-31","2015-12-28","2014-10-27","2014-08-06","2015-04-15","2015-12-31","2015-08-11","2015-01-06","2014-07-21","2016-01-01","2014-09-02","2014-06-25","2015-04-14","2014-11-03","2015-09-16","2014-06-02","2015-10-26","2014-06-10","2015-08-17","2015-02-05","2015-02-02","2015-02-20","2015-03-19","2015-04-07","2014-07-01","2015-07-10","2015-11-25","2014-04-11","2015-12-24","2015-09-15","2014-06-18","2015-12-18","2015-01-16","2015-11-23","2015-09-23","2014-03-13","2014-02-19","2015-03-02","2014-09-16","2014-07-30","2015-01-23","2014-02-17","2015-07-28","2015-09-18","2014-09-16","2015-04-09","2014-07-15","2014-10-22","2014-02-03","2015-09-01","2014-10-08","2014-05-26","2014-04-08","2014-07-16","2014-10-15","2014-09-11","2014-03-10","2014-02-05","2014-11-13","2014-04-11","2015-07-20","2015-02-13","2015-03-20","2015-12-28","2015-12-30","2014-10-28","2014-11-24","2014-06-02","2014-12-18","2015-09-18","2014-12-22","2015-07-10","2015-09-17","2014-05-13","2015-09-18","2015-06-19","2014-05-08","2015-12-24","2015-05-22","2014-06-18","2015-01-13","2014-11-14","2014-06-04","2015-03-20","2014-03-13","2015-05-15","2014-09-16","2015-03-03","2015-01-10","2015-09-28","2015-12-07","2015-05-15","2015-12-28","2014-06-10","2014-06-17","2015-03-31","2014-07-23","2014-07-21","2015-07-13","2016-01-01","2014-12-18","2015-12-30","2015-10-01","2014-08-13","2014-01-30","2015-06-24","2015-09-25","2016-01-01","2014-09-17","2015-03-06","2015-02-09","2015-10-23","2015-06-29","2015-10-20","2014-11-13","2015-02-04"],["2014-03-19","2015-06-01","2015-07-24","2015-10-05","2015-12-14","2015-02-19","2015-08-11","2014-06-17","2014-08-08","2014-05-21","2014-07-07","2014-06-11","2014-04-25","2015-09-03","2014-10-29","2015-08-10","2014-12-03","2015-12-22","2014-11-03","2015-10-21","2015-08-14","2014-04-10","2015-05-21","2014-05-01","2015-07-06","2014-11-17","2014-07-11","2015-09-02","2015-12-31","2015-04-15","2015-10-08","2014-04-11","2014-08-22","2015-11-06","2015-07-03","2015-11-19","2014-08-04","2014-12-02","2015-01-07","2015-01-09","2014-04-08","2015-02-23","2015-04-20","2014-10-06","2015-07-15","2015-11-16","2014-04-30","2015-11-30","2015-04-02","2014-04-28","2014-11-17","2015-11-30","2014-05-22","2015-10-14","2014-01-06","2014-02-19","2014-07-08","2015-03-18","2015-06-19","2015-09-15","2015-09-29","2016-01-01","2015-11-13","2014-11-07","2015-03-02","2014-10-24","2014-04-08","2015-11-25","2015-12-16","2014-11-19","2015-11-06","2014-09-12","2015-12-16","2015-06-11","2014-04-24","2015-08-12","2015-05-15","2015-12-10","2015-07-29","2014-12-03","2014-09-09","2015-04-30","2014-02-10","2014-01-28","2014-09-25","2015-12-28","2014-10-27","2014-08-06","2015-04-15","2015-08-06","2015-08-11","2015-01-06","2014-05-26","2014-09-19","2014-09-02","2014-06-25","2015-04-07","2014-11-03","2015-09-16","2014-01-27","2015-10-26","2014-06-10","2015-08-17","2015-02-05","2015-02-02","2015-01-30","2015-03-12","2015-03-17","2014-07-01","2015-07-10","2015-11-25","2014-04-11","2015-04-23","2015-09-15","2014-06-18","2015-12-04","2015-01-16","2015-10-05","2015-09-23","2014-03-13","2014-02-19","2014-12-15","2014-09-16","2014-07-23","2015-01-23","2014-02-17","2015-07-28","2015-08-14","2014-09-02","2015-04-09","2014-07-08","2014-10-01","2014-01-06","2015-07-14","2014-10-01","2014-04-28","2014-02-25","2014-06-11","2014-07-02","2014-09-11","2014-02-17","2014-02-05","2014-10-02","2014-04-11","2015-07-20","2015-02-13","2015-03-20","2014-08-25","2014-12-17","2014-10-28","2014-10-27","2014-03-31","2014-12-18","2015-09-11","2014-12-08","2015-06-19","2015-09-17","2014-05-06","2015-09-04","2015-05-22","2014-05-01","2014-01-09","2015-05-22","2014-06-18","2015-01-13","2014-11-14","2014-06-04","2015-02-20","2014-02-20","2014-08-29","2014-09-02","2015-03-03","2015-01-10","2015-09-14","2015-11-16","2015-05-08","2015-03-23","2014-06-10","2014-06-17","2015-03-31","2014-07-02","2014-07-21","2015-06-01","2014-07-11","2014-08-28","2015-11-25","2015-10-01","2014-08-06","2014-01-23","2015-06-03","2015-09-25","2015-10-23","2014-04-02","2015-02-20","2015-01-26","2015-10-23","2015-06-29","2015-10-13","2014-10-16","2015-01-07"],[1012,580,471,454,370,682,495,369,877,956,902,858,975,458,515,454,746,369,790,438,506,367,584,976,545,769,905,473,367,620,451,996,863,415,541,409,881,761,368,723,999,678,622,818,536,412,977,398,367,979,776,398,738,438,1091,1019,908,648,562,474,460,366,415,786,664,793,943,368,368,669,422,842,382,570,983,508,576,367,487,760,810,605,1049,1069,367,370,797,879,627,367,509,726,895,366,852,921,628,790,473,944,433,936,503,696,699,681,654,635,915,541,403,996,374,474,928,380,716,405,466,1025,1047,671,838,886,709,1049,523,471,838,633,901,802,1063,488,816,951,999,900,809,843,1028,1061,780,996,531,688,653,370,368,796,769,944,745,471,741,541,472,964,471,562,969,374,590,928,719,779,942,653,1025,597,838,670,722,461,391,597,370,936,929,642,893,895,538,366,745,368,458,872,1067,557,464,366,837,667,692,436,552,439,780,697],[60,35,342,39,102,31,108,3159,32,39,62,420,80,200,1365,351,102,76,35,39,37,2242,70,32,37,72,39,111,33,72,39,32,36,62,72,32,34,40,1924,34,33,40,34,38,40,34,34,37,1320,34,34,37,962,74,31,160,40,64,32,39,39.6,37,38,38,60,74,342,186,111,476,32,30,40,35,37,34,160,156,222,39,234,80,62,33,2010,40,32,38,37,836,40,34,351,2108,30,31,72,32,39,648,31,36,31,30,37,152,66,132,33,40,35,37,1190,30,30,102,39,272,35,39,30,420,33,72,40,37,33,204,111,32,76,152,155,304,80,160,224,216,570,34,140,39,217,30,39,32,38,2170,1815,37,190,320,37,62,96,160,39,80,120,155,80,2728,35,35,35,37,31,180,148,986,96,38,38.5,105,152,60,1517,32,34,39,152,31,210,2886,680,240,35,72,62,120,39,341,828,120,108,33,33,70,150,150],[60,35,342,39,102,31,108,3159,32,39,62,420,80,200,1365,351,102,76,35,39,37,2242,70,32,37,72,39,111,33,72,39,32,36,62,72,32,34,40,1924,34,33,40,34,38,40,34,34,37,1320,34,34,37,962,74,31,160,40,64,32,39,36,37,38,38,60,74,342,186,111,476,32,30,40,35,37,34,160,156,222,39,234,80,62,33,2010,40,32,38,37,836,40,34,351,2108,30,31,72,32,39,648,31,36,31,30,37,152,66,132,33,40,35,37,1190,30,30,102,39,272,35,39,30,420,33,72,40,37,33,204,111,32,76,152,155,304,80,160,224,216,570,34,140,39,217,30,39,32,38,2170,1815,37,190,320,37,62,96,160,39,80,120,155,80,2728,35,35,35,37,31,180,148,986,96,38,35,105,152,60,1517,32,34,39,152,31,210,2886,680,240,35,72,62,120,39,341,828,120,108,33,33,70,150,150],[30,35,38,39,34,31,36,39,32,39,31,35,40,40,39,39,34,38,35,39,37,38,35,32,37,36,39,37,33,36,39,32,36,31,36,32,34,40,37,34,33,40,34,38,40,34,34,37,33,34,34,37,37,37,31,32,40,32,32,39,39.6,37,38,38,30,37,38,31,37,34,32,30,40,35,37,34,40,39,37,39,39,40,31,33,30,40,32,38,37,38,40,34,39,31,30,31,36,32,39,36,31,36,31,30,37,38,33,33,33,40,35,37,35,30,30,34,39,34,35,39,30,35,33,36,40,37,33,34,37,32,38,38,31,38,40,32,32,36,38,34,35,39,31,30,39,32,38,31,33,37,38,32,37,31,32,40,39,40,40,31,40,31,35,35,35,37,31,36,37,34,32,38,38.5,35,38,30,37,32,34,39,38,31,35,37,40,40,35,36,31,30,39,31,36,40,36,33,33,35,30,30],[30,35,38,39,34,31,36,39,32,39,31,35,40,40,39,39,34,38,35,39,37,38,35,32,37,36,39,37,33,36,39,32,36,31,36,32,34,40,37,34,33,40,34,38,40,34,34,37,33,34,34,37,37,37,31,32,40,32,32,39,36,37,38,38,30,37,38,31,37,34,32,30,40,35,37,34,40,39,37,39,39,40,31,33,30,40,32,38,37,38,40,34,39,31,30,31,36,32,39,36,31,36,31,30,37,38,33,33,33,40,35,37,35,30,30,34,39,34,35,39,30,35,33,36,40,37,33,34,37,32,38,38,31,38,40,32,32,36,38,34,35,39,31,30,39,32,38,31,33,37,38,32,37,31,32,40,39,40,40,31,40,31,35,35,35,37,31,36,37,34,32,38,35,35,38,30,37,32,34,39,38,31,35,37,40,40,35,36,31,30,39,31,36,40,36,33,33,35,30,30],[30,35,38,39,34,31,36,39,32,39,31,35,40,40,39,39,34,38,35,39,37,38,35,32,37,36,39,37,33,36,39,32,36,31,36,32,34,40,37,34,33,40,34,38,40,34,34,37,33,34,34,37,37,37,31,32,40,32,32,39,39.6,37,38,38,30,37,38,31,37,34,32,30,40,35,37,34,40,39,37,39,39,40,31,33,30,40,32,38,37,38,40,34,39,31,30,31,36,32,39,36,31,36,31,30,37,38,33,33,33,40,35,37,35,30,30,34,39,34,35,39,30,35,33,36,40,37,33,34,37,32,38,38,31,38,40,32,32,36,38,34,35,39,31,30,39,32,38,31,33,37,38,32,37,31,32,40,39,40,40,31,40,31,35,35,35,37,31,36,37,34,32,38,38.5,35,38,30,37,32,34,39,38,31,35,37,40,40,35,36,31,30,39,31,36,40,36,33,33,35,30,30],[30,35,38,39,34,31,36,39,32,39,31,35,40,40,39,39,34,38,35,39,37,38,35,32,37,36,39,37,33,36,39,32,36,31,36,32,34,40,37,34,33,40,34,38,40,34,34,37,33,34,34,37,37,37,31,32,40,32,32,39,36,37,38,38,30,37,38,31,37,34,32,30,40,35,37,34,40,39,37,39,39,40,31,33,30,40,32,38,37,38,40,34,39,31,30,31,36,32,39,36,31,36,31,30,37,38,33,33,33,40,35,37,35,30,30,34,39,34,35,39,30,35,33,36,40,37,33,34,37,32,38,38,31,38,40,32,32,36,38,34,35,39,31,30,39,32,38,31,33,37,38,32,37,31,32,40,39,40,40,31,40,31,35,35,35,37,31,36,37,34,32,38,35,35,38,30,37,32,34,39,38,31,35,37,40,40,35,36,31,30,39,31,36,40,36,33,33,35,30,30],["NO","NO","NO","NO","NO","NO","NO","YES","NO","NO","NO","NO","NO","NO","YES","NO","NO","NO","NO","NO","NO","YES","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","YES","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","YES","NO","NO","NO","NO","NO","NO","NO","NO","YES","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","YES","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","YES","YES","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","YES","NO","NO","NO","NO","NO","NO","NO","YES","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","YES","NO","NO","NO","NO","NO","NO","NO","YES","NO","NO","NO","NO","NO","NO","NO","NO"],[7,0,56,0,14,0,14,560,0,0,7,77,7,28,280,56,14,7,0,0,0,630,7,0,0,7,0,14,0,7,0,0,0,7,7,0,0,0,357,0,0,0,0,0,0,0,0,0,273,0,0,0,217,7,0,28,0,7,0,0,0,0,0,0,7,7,56,35,14,105,0,0,0,0,0,0,21,21,35,0,35,7,7,0,462,0,0,0,0,147,0,0,56,469,0,0,7,0,0,126,0,0,0,0,0,21,7,21,0,0,0,0,245,0,0,14,0,49,0,0,0,77,0,7,0,0,0,35,14,0,7,21,28,49,7,28,42,35,105,0,21,0,42,0,0,0,0,490,378,0,28,63,0,7,14,21,0,7,14,28,7,714,0,0,0,0,0,28,21,259,14,0,0,14,21,7,280,0,0,0,21,0,42,539,112,35,0,7,7,21,0,70,168,14,14,0,0,7,28,28],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3.6,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3.5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],["1","0","2","0","1","0","1","2","0","0","1","2","1","1","2","2","1","1","0","0","0","2","1","0","0","1","0","1","0","1","0","0","0","1","1","0","0","0","2","0","0","0","0","0","0","0","0","0","2","0","0","0","2","1","0","1","0","1","0","0","0","0","0","0","1","1","2","2","1","2","0","0","0","0","0","0","1","1","2","0","2","1","1","0","2","0","0","0","0","2","0","0","2","2","0","0","1","0","0","2","0","0","0","0","0","1","1","1","0","0","0","0","2","0","0","1","0","2","0","0","0","2","0","1","0","0","0","2","1","0","1","1","1","2","1","1","2","2","2","0","1","0","2","0","0","0","0","2","2","0","1","2","0","1","1","1","0","1","1","1","1","2","0","0","0","0","0","1","1","2","1","0","0","1","1","1","2","0","0","0","1","0","2","2","2","2","0","1","1","1","0","2","2","1","1","0","0","1","1","1"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>user_id<\/th>\n      <th>Frequency<\/th>\n      <th>Last_pay<\/th>\n      <th>First_pay<\/th>\n      <th>Recency<\/th>\n      <th>Paid<\/th>\n      <th>Due<\/th>\n      <th>Max_Paid<\/th>\n      <th>Max_Due<\/th>\n      <th>Avg_Paid<\/th>\n      <th>Avg_due<\/th>\n      <th>Purchased_Again<\/th>\n      <th>Tenure<\/th>\n      <th>Diff<\/th>\n      <th>Same_month_churn<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[1,2,6,7,8,9,10,11,14]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->



# Next Steps

There is much more information that can be mined from this dataset in order to get a better understanding about the customer behaviour. As next step, I would break the customer base into segments in order to look at them under every possible perspective. This could reveal customer segment with a special behaviour that would lead us to relevant actions. 
 




