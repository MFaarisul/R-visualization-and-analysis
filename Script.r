# Overview
# This notebook will focus on data visualization with some analysis to understand the condition inside the business and get a sense of what happend in the past. All of this work are using **R language** instead of Python
# There are 18 unique visualizations throughout this notebook

# Importing Library and Loading the Data
install.packages('hexbin')
install.packages('tidyverse')
install.packages('htmlwidgets')
install.packages('DT')
install.packages('plyr')
install.packages('waffle', repos = "https://cinc.rud.is")

library(plyr)
library(tidyverse)
library(lubridate)
library(treemapify)
library(waffle)

Sys.setlocale(locale="C")


df <- read_csv("Data/Sample - Superstore.csv")
head(df)

cat('Total of rows:', nrow(df))
cat('\nTotal of columns:', ncol(df))


# Data Visualization

# Visualization #1
df <- df %>%
    mutate(`Order Date` = mdy(`Order Date`),
           `Ship Date` = mdy(`Ship Date`),
           'Shipping Speed' = `Ship Date` - `Order Date`)

df %>%
    group_by(`Ship Mode`) %>%
    summarize(mean=mean(`Shipping Speed`)) %>%
    ggplot(aes(x=reorder(`Ship Mode`, -`mean`),
               y=`mean`, fill=reorder(`Ship Mode`,
                                      `mean`)))+
    geom_bar(stat='identity')+
    coord_flip()+
    geom_text(aes(label = paste0(round(mean, 1),
                                 ' day')),
              hjust = -0.5, size=5,
              fontface='bold')+
    scale_y_continuous(limits = c(0,6))+
    theme_classic()+
    labs(title='Comparison of Shipping Speed\nfor Each Ship Mode',
        x='Ship Mode',
        y='Mean (day)',
        fill='Ship Mode')+
    theme(plot.title = element_text(size = 20,
                                    face = "bold",
                                    hjust=0.5))

# Visualization #2
df %>% 
    ggplot(aes(x=`Ship Mode`, y=`Segment`, shape=`Segment`, color=`Segment`))+
    geom_count()+
    scale_size(range = c(3,18))+
    theme_light()+
    labs(title='Ship Mode per Segment')+
    theme(plot.title = element_text(size = 20, face = "bold", hjust=0.5))

# Visualization #3
customer <- df %>%
    group_by(`Customer Name`) %>%
    summarize('Total Item'=sum(Quantity)) %>%
    arrange(-`Total Item`) %>%
    slice(1:5)

customer %>%
    ggplot(aes(x=reorder(`Customer Name`, `Total Item`), y=`Total Item`, color=`Customer Name`))+
    geom_point(stat='identity', size=12)+
    geom_segment(aes(y=0, xend=`Customer Name`, yend=`Total Item`))+
    geom_text(aes(label=`Total Item`), color='white', size=5, vjust=0.5, fontface='bold')+
    theme_classic()+
    labs(title='Top 5 Customers\nby Total Purchased Items')+
    theme(plot.title=element_text(size=20, hjust=0.5, face='bold'))+
    scale_y_continuous(limits=c(0, 155))+
    coord_flip()

#We also want to know the segment of the top 5 customers, so up next we will perform some data processing to answer that
customerName <- customer$'Customer Name'
segment <- df[df$'Customer Name' %in% customerName,]

segment %<>%
    group_by(`Customer Name`, `Segment`) %>%
    summarize()

left_join(x=customer, y=segment, by='Customer Name')

# Visualization #4
customerSales <- df %>%
    group_by(`Customer Name`) %>%
    summarize('Total Payment'=sum(Sales), 'Total Item'=sum(Quantity)) %>%
    arrange(-`Total Payment`) %>%
    slice(1:5)

customerSales %>%
    ggplot(aes(x=reorder(`Customer Name`, `Total Payment`), y=`Total Payment`, fill=`Customer Name`))+
    geom_bar(stat='identity')+
    geom_text(aes(label=paste0(round(`Total Payment`), '\nUSD')), color='white', size=5, vjust=2, fontface='bold')+
    theme_classic()+
    labs(title='Top 5 Customers\nby Total Payment')+
    theme(plot.title=element_text(size=20, hjust=0.5, face='bold'))+
    scale_y_continuous(limits=c(0, 26000))

customerSalesName <- customerSales$'Customer Name'
customerSalesSegment <- df[df$'Customer Name' %in% customerSalesName,]

customerSalesSegment %<>%
    group_by(`Customer Name`, `Segment`) %>%
    summarize()

left_join(x=customerSales, y=customerSalesSegment, by='Customer Name')

# Visualization #5
state <- df %>%
    group_by(`State`) %>%
    summarize(count=n()) %>%
    arrange(-`count`)

state$region <- tolower(state$State)

stateMap <- merge(map_data('state'), state, by='region')

stateMap %>%
    ggplot(aes(long, lat, group=group))+
    geom_polygon(aes(fill=count), color = 'white')+
    scale_fill_viridis_b(option = 'E')+
    labs(title='Distribution of Customers by State')+
    theme_classic()+
    theme(plot.title=element_text(size=20, face='bold', hjust=0.5))

# Visualization #6
polarPlotData <- slice(state, 1:5)[c('State', 'count')] %>%
    mutate(position=count/2)

polarPlotData %>%
    ggplot(aes(x=reorder(State, -count), y=count, fill=State, order_by=count))+
    geom_bar(stat='identity')+
    geom_text(aes(y=position, label=count), color='white')+
    coord_polar()+
    labs(title='Top 5 State by Total Customer',
        x='State')+
    theme_light()+
    theme(plot.title=element_text(size=20, face='bold', hjust=0.5),
          axis.text.x=element_text(face='bold'))

# Visualization #7
reg <- df %>%
    group_by(Region) %>%
    summarize(count=n())

reg %<>% 
    mutate(position=cumsum(reg$count)-(0.5*count),
           percent=(count/sum(reg$count)*100)) %>%
    # To be able to use position_stack in geom_text
    as.data.frame()

reg %>%
    ggplot(aes(x='', y=count, fill=Region))+
    geom_bar(stat='identity', width=1)+
    geom_text(aes(label=paste0(round(percent, 2), '%')), size=8, fontface='bold', color='white', position=position_stack(0.5))+
    coord_polar(theta='y', start=0)+
    labs(title='Region Distribution')+
    theme_void()+
    theme(plot.title=element_text(size=20, hjust=0.5, face='bold'))

# Visualization #8
category <- df %>%
    group_by(`Category`, `Sub-Category`) %>%
    summarize(count=n())

category %>% 
    ggplot(aes(area=count, label=`Sub-Category`, fill=Category, subgroup=Category))+
    geom_treemap()+
    geom_treemap_subgroup_border(colour = "white", size = 5)+
    geom_treemap_subgroup_text(color='white', alpha=0.3, size=25, , place='center', fontface='bold')+
    geom_treemap_text(color='white', place='center', size=15)

# Visualization #9
product <- df %>%
    group_by(`Product Name`) %>%
    summarize(count=n()) %>%
    arrange(-`count`) %>%
    slice(1:5)

hsize <- 2

product %>%
    ggplot(aes(x=hsize, y=count, fill=`Product Name`))+
    geom_bar(stat='identity', color='white')+
    geom_text(aes(label=`count`), position=position_stack(0.5), color='white', size=6, fontface='bold')+
    coord_polar(theta='y')+
    xlim(c(0, hsize+1))+
    labs(title='Top 5 Product Name \nby Total Purchase')+
    theme_void()+
    theme(plot.title=element_text(size=20, hjust=0.5, face='bold'))

productName <- product$'Product Name'
productNameID <- df[df$'Product Name' %in% productName,][c('Product Name', 'Product ID', 'Category')]

totalType <- productNameID %>%
    group_by(`Product Name`, `Product ID`) %>%
    summarize(count=n()) %>%
    group_by(`Product Name`) %>%
    summarize('Total Type'=n())

productCat <- productNameID %>%
    group_by(`Product Name`, `Category`) %>%
    summarize()

left_join(x=totalType, y=productCat, by='Product Name')

# As you can see from above table, all of top 5 product come from office supplies. The tabel also tells us about how many types do the products have
# For instance:
# Staples have 10 different product IDs which correspond to 10 different types of staple products

# Visualization #10
df %>%
    group_by(Segment, `Sub-Category`) %>%
    summarize("Total Payment"=sum(Sales)) %>%
    arrange(`Segment`, -`Total Payment`) %>%
    slice(1)

df %>%
    group_by(Segment, `Sub-Category`) %>%
    summarize("Total Payment"=sum(Sales)) %>%
    ggplot(aes(x=Segment, y=`Sub-Category`, fill=`Total Payment`))+
    scale_fill_viridis_b(option = 'D')+
    geom_tile(color='white')+
    geom_text(aes(label=paste0(round(`Total Payment`, 2))), color='white', fontface='bold')+
    labs(title='Segment vs Sub-Category\nby Sales')+
    theme_classic()+
    theme(plot.title=element_text(size=20, face='bold', hjust=0.5))

# Visualization #11
df %>%
    ggplot(aes(x=Sales, y=Profit, color=Quantity))+
    geom_point(size=3)+
    geom_rug()+
    labs(title='Sales vs Profit')+
    theme_light()+
    theme(plot.title=element_text(size=20, face='bold', hjust=0.5))

# Visualization #12
df %>%
    ggplot(aes(x=Sales, y=Profit))+
    geom_point(size=3, alpha=0.4)+
    geom_smooth(aes(color=Category), method='gam', fullrange = TRUE)+
    facet_wrap(~Category)+
    labs(title='Sales vs Profit\nfor Each Category')+
    theme_light()+
    theme(plot.title=element_text(size=20, face='bold', hjust=0.5))

# Visualization #13
df %>%
    group_by(Category) %>%
    summarize('Max Discount'=max(Discount)*10) %>%
    ggplot(aes(fill=Category, values=`Max Discount`))+
    geom_waffle(n_rows = 2, size = 4, color = "white", flip=TRUE)+
    geom_text(aes(x=1.5, y=2.5, label=paste0(`Max Discount`*10, '%')),
              size=15, alpha=0.5, fontface='bold')+
    facet_wrap(~Category)+
    labs(title='Max Discount\nfor Each Category')+
    theme_void()+
    theme(plot.title=element_text(size=20, face='bold', hjust=0.5))

# Here is the sub-category that holds the biggest discount
df %>%
    group_by(Category, `Sub-Category`) %>%
    summarize('Max Discount'=max(Discount)*100) %>%
    arrange(-`Max Discount`) %>%
    slice(1)

# Visualization #14
df %>%
    ggplot(aes(x=Discount, y=Profit))+
    geom_bin2d(size=3)+
    labs(title='Discount vs Profit')+
    theme_light()+
    theme(plot.title=element_text(size=20, face='bold', hjust=0.5))

# Visualization #15
df %>%
    ggplot(aes(x=Discount, y=Profit, color=Category, fill=Category))+
    geom_histogram(stat='identity', bins=10)+
    facet_wrap(~`Sub-Category`)+
    labs(title='Discount vs Profit\nby Sub-Categories')+
    theme_light()+
    theme(plot.title=element_text(size=20, face='bold', hjust=0.5))

# Visualization #16
df %>%
    ggplot(aes(x=Discount, y=Sales))+
    geom_hex(size=3)+
    labs(title='Discount vs Sales')+
    theme_light()+
    theme(plot.title=element_text(size=20, face='bold', hjust=0.5))

# Visualization #17
df_disc <- df[df$Discount >= 0.25, ]

df_disc %>%
    ggplot(aes(x=Discount, fill=`Sub-Category`))+
    geom_bar()+
    facet_wrap(~Category)+
    labs(title='Total discounted item above 25%\nby Categories')+
    theme_light()+
    theme(plot.title=element_text(size=20, face='bold', hjust=0.5))

# Visualization #18
df %>%
    ggplot(aes(x=Quantity, y=Profit))+
    geom_density_2d()+
    stat_density2d(aes(fill = ..level..), geom = "polygon")+
    labs(title='Quantity vs Profit')+
    theme_light()+
    theme(plot.title=element_text(size=20, face='bold', hjust=0.5))

