library("gganimate")
library(ggplot2)
library(gapminder)
library("gifski")
library("png")
library("ggrepel")
library("dplyr")
library("mapdata")
library(transformr)
library(tweenr)
ggplot(gapminder, aes(x = gdpPercap, y=lifeExp, size = pop, colour = country)) + geom_point(aes(x = gdpPercap, y=lifeExp, size = pop, colour = country))

#alpha transparentno
#scale colour virdis 
#kolory option = "B"
#drugi geom point to Polska
# geom_text(data=gapminder[1225:1236, ], colour="red",label="Polska",show.legend="FALSE")+


p <- ggplot(
  gapminder, 
  aes(x = gdpPercap, y=lifeExp, size = pop, colour = continent)
  ) +
  geom_point(show.legend = TRUE,alpha = 0.7) +
  geom_point(data=gapminder[1225:1236, ], colour="green", size=5)+
  scale_color_viridis_d(option="B" ) +
  scale_size(range = c(1, 15)) +
  scale_x_log10() +
  labs(x = "PKB", y = "Oczekwiana d逝go 篡cia")+
  theme_grey(base_size = 18)

#wszytsko razem jak sie porusza

p + transition_time(year) +
  labs(title = "Year: {frame_time}")+
  view_follow(fixed_y = TRUE)+
  theme_grey(base_size = 18)

#podzielone na kontynety

p + facet_wrap(~continent) +
  transition_time(year) +
  labs(title = "Year: {frame_time}")

#olska
Polska=gapminder[1225:1236,]
Polska2=Polska[5:10,]
Rwanda=gapminder[1285:1296,]
Japan=gapminder[793:804,]

moze=gapminder[c(793:804,1285:1296,1225:1236),]
sama_pl<- ggplot(Polska2,
  aes(x = gdpPercap, y=lifeExp),xlim=c(min(Polska2$lifeExp),max(Polska2$lifeExp)),ylim=c(min(Polska2$gdpPercap),max(gdpPercap)+5000))+
  geom_line(size=7,show.legend=FALSE,col="green")+
  geom_point()+ 
  geom_text(data=Polska2,label=floor(Polska2$year),size=15,show.legend=FALSE,aes(group = seq_along(Polska2$year)),hjust = 0, vjust = 0)+
  scale_size(range = c(1, 15)) +
  scale_x_log10() +
  labs(x = "PKB",size=100 ,y = "Oczekwiana d逝go 篡cia",size=100)+
  theme_grey(base_size = 60)+ggtitle("Polska")

ani_pl<-sama_pl+transition_reveal(year)

animate(ani_pl, height = 1000, width =2500,renderer = gifski_renderer(loop = F),fps=4)

moja_pl<- ggplot(moze,
  aes(x = gdpPercap, y=lifeExp,group=country)
  ) +
  geom_line(data=Polska,size=2,show.legend=FALSE,col="green")+
  geom_line(data=Rwanda,size=2,show.legend=FALSE,col="yellow")+
  geom_line(data=Rwanda,size=2,show.legend=FALSE,col="blue")+
  scale_color_viridis_d(option="E") +
  geom_point()+ 
  #geom_text_repel(aes(label=year), box.padding = unit(0.9, "lines"),show.legend=FALSE,seed=123) +
  geom_text(data=Polska,label=floor(Polska$year),size=7,show.legend=FALSE,aes(group = seq_along(Polska$year)),hjust = 0, vjust = 0)+
  scale_size(range = c(1, 15)) +
  scale_x_log10() +
  labs(x = "PKB",size=100 ,y = "Oczekwiana d逝go 篡cia",size=100)+
  theme_grey(base_size = 60)

ani_pl<-moja_pl+
transition_reveal(year)
animate(ani_pl, height = 1000, width =1000,renderer = gifski_renderer(loop = F),fps=3)

######################serce

N=100
t=seq(0,2*3.14,by=0.01)
x=16*sin(t)*sin(t)*sin(t)
y=13*cos(t)-5*cos(2*t)-2*cos(3*t)-cos(4*t)
x_i=matrix(nrow=length(x),ncol=N)
y_i=matrix(nrow=length(x),ncol=N)
rep=matrix(nrow=length(x),ncol=N)

for (i in 1:N){
y_i[,i]=i*y
x_i[,i]=i*x
rep[,i]=rep(i,length(x))
}
x_i=as.vector(x_i)
y_i=as.vector(y_i)
rep=as.vector(rep)
s=data.frame(x=x_i,y=y_i,grupa=rep)

serce<-ggplot(s,
  aes(x = x, y=y,group=grupa)
  ) +geom_point(show.legend=FALSE,col="red")+
 scale_color_viridis_d(option="B" )
serce

ani_s<-serce+
transition_states(s$grupa)
animate(ani_s)
##################swiat populacja rosnie
swiat<-map_data("world")
map<-ggplot()+geom_polygon(data=swiat,aes(x=long,y=lat,group=group),fill="lightblue")+ggtitle("Jak roie populacja iata")+
labs(y="szeroko geograficzna",x="d逝go geograficzna")
gap=data.frame(region=gapminder$country,pop=gapminder$pop,year=gapminder$year)
a=distinct(swiat,region,.keep_all=TRUE)
a=a[,c(1,2,5)]

moj_r_join=inner_join(gap,a,by="region")

mapka<-map+geom_point(data=moj_r_join, aes(x=moj_r_join$long,y=moj_r_join$lat,color=moj_r_join$region),size=(moj_r_join$pop/30000000),show.legend=FALSE)+
scale_size(range = c(1, 6))+
scale_color_viridis_d(option="C")

mapka + transition_time(year)+labs(title = "Rok: {frame_time}")

#############################swiat pkb sie zmienia
swiat<-map_data("world")
map<-ggplot(data=swiat)+geom_polygon(aes(x=long,y=lat,group=group))+
ggtitle("Jak roie populacja iata")

gap=data.frame(region=gapminder$country,gdp=gapminder$gdpPercap,year=gapminder$year)
a=swiat[,c(1,2,3,5)]
moj_r_join=inner_join(gap,a,by="region")

nowy_swiat<-map+geom_polygon(data = moj_r_join[1:410,], aes(x=long,y=lat,group=group,fill = moj_r_join[1:410,]$gdp),fill="red")+
scale_color_viridis_d(option="A")

################s逝pki
#pytanie 1.1 ile dziewczynek konczy podstawowke- 10% poprawnosci max
#pytanie 2.9  ile dzieci 1 rok zaszczepionyvh- 30%
#ile dizeci zostalo zaszczepionych jaki procent

data1=data.frame(quest=c(1,2,3,4),resp=c(0,0,0,0),gr=c(1,2,1,2),frame=rep("a",4))
data2=data.frame(quest=c(1,2,3,4),resp=c(60,10,80,30),gr=c(1,2,1,2),frame=rep("b",4))
data=rbind(data1,data2)

ts <- list(data1, data2, data1)
tf <- tween_states(ts, tweenlength = 0.02, statelength = 0.001, ease = c('cubic-in-out'), nframes = 30)
tf=tf[1:68,]

p=ggplot(tf, aes(x=quest, y=resp, fill=resp,group=quest)) + 
geom_bar(stat = "Identity",position="Identity", alpha = 0.66)+
#transition_reveal(resp)+  
#ease_aes('linear')+
facet_wrap(~gr)+
scale_fill_manual(values=c("red", "blue"))

############
data1=data.frame(quest=c("Pytanie1","Pytanie1","Pytanie2","Pytanie2"),resp=c(0,0,0,0),
gr=c("prawid這wa odp","udzieli這 prawid這wej odp","prawid這wa odp",
"udzieli這 prawid這wej odp"),frame=rep("a",4))
data2=data.frame(quest=c("Pytanie1","Pytanie1","Pytanie2","Pytanie2"),
resp=c(60,10,80,30),gr=c("prawid這wa odp","udzieli這 prawid這wej odp","prawid這wa odp",
"udzieli這 prawid這wej odp"),frame=rep("b",4))
data=rbind(data1,data2)

ts <- list(data1, data2, data1)
tf <- tween_states(ts, tweenlength = 0.02, statelength = 0.001, ease = c('cubic-in-out'), nframes = 30)
tf=tf[1:68,]

p=ggplot(tf, aes(x=gr,y=resp, fill=factor(gr),group=factor(gr)),xlab="",color=factor(gr)) + 
geom_bar(stat = "Identity",position="Identity")+
transition_reveal(resp)+  
ease_aes('linear')+
facet_wrap(~quest,drop=TRUE)+
scale_fill_manual(values=c("blue", "green"),labels=c("prawid這wa odp","ilu udzieli這 takiej"))+
xlab(element_blank())+
ylab("Procent")+theme_grey(base_size = 28)+
theme(legend.position="top",legend.title=element_blank(), 
strip.text.x = element_blank(),axis.text.x=element_blank())

animate(p, fps =4, renderer = gifski_renderer(loop = F),duration = 15, width = 800, height = 600)



slupki<-ggplot(dane,aes(x=pyt,y=odp,fill=pyt,group=pyt))+geom_col()+ 
scale_fill_distiller(palette = "Set2", direction = 1) +
theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "white"),
    panel.ontop = TRUE
  )
slupki+transition_states(odp),renderer = gifski_renderer(loop = F),fps=10)

