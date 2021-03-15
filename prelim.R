rm(list=ls(all=TRUE))  ## efface les données
setwd('~/thib/projects/conftime/') 
source('~/thib/projects/tools/R_lib.r')
library('pander')

data <- read_csv('data_Maniscalco_2017_expt1.csv') %>%
    filter(!(Subj_idx %in% c(3,6,9,22))) %>%
    filter(is.nan(RT_dec) == FALSE) %>%
    filter(is.nan(RT_conf) == FALSE) %>%
    mutate(acc = (Stimulus == Response)) %>%
    filter(acc == 1, RT_dec < 2, RT_conf <2)
d.dec <- data %>%
    select(Subj_idx, RT_dec) %>%
    group_by(Subj_idx) %>%
    summarise(RT_dec = quantile(RT_dec, c(1:9)/10), q =  c(1:9)/10)
d.conf <- data %>%
    select(Subj_idx, RT_conf) %>%
    group_by(Subj_idx) %>%
    summarise(RT_conf = quantile(RT_conf, c(1:9)/10), q =  c(1:9)/10) 
d <- full_join(d.dec, d.conf) %>%
  group_by(q) %>%
  summarise(RT_conf = mean(RT_conf), RT_dec = mean(RT_dec))

d.dec <- data %>%
    select(RT_dec, Subj_idx) %>%
    rename(RT = RT_dec) %>%
    mutate(type = 'dec')
d.conf <- data %>%
  select(RT_conf, Subj_idx) %>%
  rename(RT = RT_conf) %>%
  mutate(type = 'conf')
d.dist <- rbind(d.dec, d.conf)

d.corr <- data %>%
  group_by(Subj_idx) %>%
  summarise(corr = cor(RT_conf,RT_dec, method="pearson"))

plot <- ggplot(data = d.dist, mapping = aes(x = RT, colour = type)) +
  geom_density() +
  facet_wrap(~Subj_idx, scale = 'free')   
plot

plot <- ggplot(data = d, mapping = aes(x = RT_dec, y = RT_conf)) + 
  geom_line() +
  geom_point() +
  geom_smooth(method='lm', se = FALSE)  
plot

pandoc.table(d, style = 'rmarkdown')

plot <- ggplot(data = d.corr, mapping = aes(x = Subj_idx, y = corr)) +
    geom_point() +
    geom_hline(aes(yintercept = mean(corr)), color="blue") +
    ylim(-1,1) +
    annotate(geom = 'text', x = 15, y = 0.8, label = paste('mean correlation = ', round(mean(d.corr$corr),2),sep = ''))
print(plot)

pandoc.table(d.corr, style = 'rmarkdown')

data <- read_csv('data_Gherman_2018.csv') %>%
    filter(is.nan(RT_dec) == FALSE) %>%
    filter(is.nan(RT_conf) == FALSE) %>%
    filter(Excluded_subj == 0) %>%
    mutate(acc = (Stimulus == Response)) %>%
    filter(acc == 1, RT_conf < 2, RT_dec < 2)
d.dec <- data %>%
      select(Subj_idx, RT_dec) %>%
      group_by(Subj_idx) %>%
      summarise(RT_dec = quantile(RT_dec, c(1:9)/10), q =  c(1:9)/10)
d.conf <- data %>%
      select(Subj_idx, RT_conf) %>%
      group_by(Subj_idx) %>%
      summarise(RT_conf = quantile(RT_conf, c(1:9)/10), q =  c(1:0)/100)
d <- full_join(d.dec, d.conf) %>%
    group_by(q) %>%
    summarise(RT_conf = mean(RT_conf), RT_dec = mean(RT_dec))

d.dec <- data %>%
      select(RT_dec, Subj_idx) %>%
      rename(RT = RT_dec) %>%
      mutate(type = 'dec')
d.conf <- data %>%
    select(RT_conf, Subj_idx) %>%
    rename(RT = RT_conf) %>%
    mutate(type = 'conf')
d.dist <- rbind(d.dec, d.conf)

d.corr <- data %>%
  group_by(Subj_idx) %>%
  summarise(corr = cor(RT_conf,RT_dec, method="pearson"))

plot <- ggplot(data = d.dist, mapping = aes(x = RT, colour = type)) +
  geom_density() +
  facet_wrap(~Subj_idx, scale = 'free')   
plot

plot <- ggplot(data = d, mapping = aes(x = RT_dec, y = RT_conf)) + 
  geom_line() +
  geom_point() +
  geom_smooth(method='lm', se = FALSE)  
plot

pandoc.table(d, style = 'rmarkdown')

plot <- ggplot(data = d.corr, mapping = aes(x = Subj_idx, y = corr)) +
   geom_point() +
   geom_hline(aes(yintercept = mean(corr)), color="blue") +
   ylim(-1,1) +
   annotate(geom = 'text', x = 15, y = 0.8, label = paste('mean correlation = ', round(mean(d.corr$corr),2),sep = ''))

plot

pandoc.table(d.corr, style = 'rmarkdown')

data <- read_csv('data_Hainguerlot_2018.csv') %>%
     filter(is.nan(RT_dec) == FALSE) %>%
     filter(is.nan(RT_conf) == FALSE) %>%
     mutate(acc = (Stimulus == Response)) %>%
     filter(acc == 1, RT_dec < 2, RT_conf < 2)
d.dec <- data %>%
     select(Subj_idx, RT_dec) %>%
     group_by(Subj_idx) %>%
     summarise(RT_dec = quantile(RT_dec, c(1:9)/10), q =  c(1:9)/10)
 d.conf <- data %>%
     select(Subj_idx, RT_conf) %>%
     group_by(Subj_idx) %>%
     summarise(RT_conf = quantile(RT_conf, c(1:9)/10), q =  c(1:9)/10)
 d <- full_join(d.dec, d.conf) %>%
   group_by(q) %>%
   summarise(RT_conf = mean(RT_conf), RT_dec = mean(RT_dec))

 d.dec <- data %>%
     select(RT_dec, Subj_idx) %>%
     rename(RT = RT_dec) %>%
     mutate(type = 'dec')
 d.conf <- data %>%
   select(RT_conf, Subj_idx) %>%
   rename(RT = RT_conf) %>%
   mutate(type = 'conf')
 d.dist <- rbind(d.dec, d.conf)

 d.corr <- data %>%
   group_by(Subj_idx) %>%
   summarise(corr = cor(RT_conf,RT_dec, method="pearson"))

plot <- ggplot(data = d.dist, mapping = aes(x = RT, colour = type)) +
  geom_density() +
  facet_wrap(~Subj_idx, scale = 'free')
plot

plot <- ggplot(data = d, mapping = aes(x = RT_dec, y = RT_conf)) + 
  geom_line() +
  geom_point() +
  geom_smooth(method='lm', se = FALSE)  
plot

pandoc.table(d, style = 'rmarkdown')

plot <- ggplot(data = d.corr, mapping = aes(x = Subj_idx, y = corr)) +
   geom_point() +
   geom_hline(aes(yintercept = mean(corr)), color="blue") +
   ylim(-1,1) +
   annotate(geom = 'text', x = 15, y = 0.8, label = paste('mean correlation = ', round(mean(d.corr$corr),2),sep = ''))

plot

pandoc.table(d.corr, style = 'rmarkdown')

data <- read_csv('data_Hainguerlot_unpub.csv') %>%
     filter(is.nan(RT_dec) == FALSE) %>%
     filter(is.nan(RT_conf) == FALSE) %>%
     mutate(acc = (Stimulus == Response)) %>%
     filter(acc == 1, RT_dec < 2, RT_conf < 2)
d.dec <- data %>%
     select(Subj_idx, RT_dec) %>%
     group_by(Subj_idx) %>%
     summarise(RT_dec = quantile(RT_dec, c(1:9)/10), q =  c(1:9)/10)
 d.conf <- data %>%
     select(Subj_idx, RT_conf) %>%
     group_by(Subj_idx) %>%
     summarise(RT_conf = quantile(RT_conf, c(1:9)/10), q =  c(1:9)/10)
 d <- full_join(d.dec, d.conf) %>%
   group_by(q) %>%
   summarise(RT_conf = mean(RT_conf), RT_dec = mean(RT_dec))

 d.dec <- data %>%
     select(RT_dec, Subj_idx) %>%
     rename(RT = RT_dec) %>%
     mutate(type = 'dec')
 d.conf <- data %>%
   select(RT_conf, Subj_idx) %>%
   rename(RT = RT_conf) %>%
   mutate(type = 'conf')
 d.dist <- rbind(d.dec, d.conf)

 d.corr <- data %>%
   group_by(Subj_idx) %>%
   summarise(corr = cor(RT_conf,RT_dec, method="pearson"))

plot <- ggplot(data = d.dist, mapping = aes(x = RT, colour = type)) +
  geom_density() +
  facet_wrap(~Subj_idx, scale = 'free')
plot

plot <- ggplot(data = d, mapping = aes(x = RT_dec, y = RT_conf)) + 
  geom_line() +
  geom_point() +
  geom_smooth(method='lm', se = FALSE)  
plot

pandoc.table(d, style = 'rmarkdown')

plot <- ggplot(data = d.corr, mapping = aes(x = Subj_idx, y = corr)) +
   geom_point() +
   geom_hline(aes(yintercept = mean(corr)), color="blue") +
   ylim(-1,1) +
   annotate(geom = 'text', x = 15, y = 0.8, label = paste('mean correlation = ', round(mean(d.corr$corr),2),sep = ''))

plot

pandoc.table(d.corr, style = 'rmarkdown')

data <- read_csv('data_Locke_2020.csv') %>%
   filter(is.nan(RT_dec) == FALSE) %>%
   filter(is.nan(RT_conf) == FALSE) %>%
   filter(Training == 0) %>%
   mutate(acc = (Stimulus == Response)) %>%
   filter(acc == 1) %>%
   filter(Condition == 1) %>%
   filter(RT_dec < 2, RT_conf <2) 

d.dec <- data %>%
     select(Subj_idx, RT_dec) %>%
     group_by(Subj_idx) %>%
     summarise(RT_dec = quantile(RT_dec, c(1:9)/10), q =  c(1:9)/10)
 d.conf <- data %>%
     select(Subj_idx, RT_conf) %>%
     group_by(Subj_idx) %>%
     summarise(RT_conf = quantile(RT_conf, c(1:9)/10), q =  c(1:9)/10)
 d <- full_join(d.dec, d.conf) %>%
   group_by(q) %>%
   summarise(RT_conf = mean(RT_conf), RT_dec = mean(RT_dec))

 d.dec <- data %>%
     select(RT_dec, Subj_idx) %>%
     rename(RT = RT_dec) %>%
     mutate(type = 'dec')
 d.conf <- data %>%
   select(RT_conf, Subj_idx) %>%
   rename(RT = RT_conf) %>%
   mutate(type = 'conf')
 d.dist <- rbind(d.dec, d.conf)

 d.corr <- data %>%
   group_by(Subj_idx) %>%
   summarise(corr = cor(RT_conf,RT_dec, method="pearson"))

plot <- ggplot(data = d.dist, mapping = aes(x = RT, colour = type)) +
  geom_density() +
  facet_wrap(~ Subj_idx, scale = 'free') 
plot

plot <- ggplot(data = d, mapping = aes(x = RT_dec, y = RT_conf)) + 
  geom_line() +
  geom_point() +
  geom_smooth(method='lm', se = FALSE)  
plot

pandoc.table(d, style = 'rmarkdown')

plot <- ggplot(data = d.corr, mapping = aes(x = Subj_idx, y = corr)) +
   geom_point() +
   geom_hline(aes(yintercept = mean(corr)), color="blue") +
   ylim(-1,1) +
   annotate(geom = 'text', x = 6, y = 0.8, label = paste('mean correlation = ', round(mean(d.corr$corr),2),sep = ''))

plot

pandoc.table(d.corr, style = 'rmarkdown')

x_norm <- rnorm(500, mean = 300, sd = 150) 
x_weib <- rweibull(500, shape = 15, scale = 300)
data <- data.frame(norm = x_norm, weib = x_weib)
dec_norm <- quantile(x_norm, c(1:9)/10)
dec_weib <- quantile(x_weib, c(1:9)/10)
d <- data.frame(q = c(1:9)/10, norm = dec_norm, weib = dec_weib)

plot <- ggplot(data = d, mapping = aes(x = dec_norm, y = dec_weib)) + 
  geom_line() +
  geom_point() +
  geom_smooth(method='lm', se = FALSE)  
plot

data <- data.frame(norm = x_norm, weib = x_weib)
d.dist <- data %>%
  select(-weib) %>%
  mutate(type = 'norm') %>%
  rename(x = norm)
d.dist2 <- data %>%
  select(-norm) %>%
  mutate(type = 'weib') %>%
  rename(x = weib)
data <- rbind(d.dist, d.dist2)

plot <- ggplot(data = data, mapping = aes(x = x, colour = type)) +
  geom_density() 
plot

data <- data.frame(norm = x_norm, weib = x_weib)
dec_norm <- quantile(x_norm, c(1:100)/100)
dec_weib <- quantile(x_weib, c(1:100)/100)
d <- data.frame(q = c(1:100)/100, norm = dec_norm, weib = dec_weib)

plot <- ggplot(data = d, mapping = aes(x = dec_norm, y = dec_weib)) + 
  geom_line() +
  geom_point() +
  geom_smooth(method='lm', se = FALSE)  
plot
