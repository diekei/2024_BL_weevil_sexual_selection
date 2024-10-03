### INTRODUCTION #####

# Welcome. For the description of the project please visit: https://github.com/diekei/2024_EE_weevil_sexual_selection
# Article is available at:


### LIBRARY #####
remotes::install_github('https://github.com/J0vid/Colormesh')
remotes::install_github('StevenVB12/patternize')
library(Colormesh)
library(patternize)
library(viridis)
library(ggstatsplot)
library(tidyverse)
library(ggplot2)
library(colorblindcheck)
library(GGally)
library(ggside)
library(car)
library(rstatix)
library(DHARMa)
library(ggsignif)
# attention! please run the code one by one, step by step


### PRE-ANALYSIS - IMAGE STANDARISATION #####

# creating landmarks
'STEP:
1. The number of landmarks can be customised
2. Landmarks should be positioned in exact order for all
3. Put fixed landmark first then semilandmark, could be clockwise or counterclockwise, BE CONSISTENT
4. For each landmark, they will ask your confirmation'

specimen.factors.lft <- read.csv('specimen.csv', header = F)

specimen.LM.lft <- landmark.images(imagedir = 'images/standardised/standardised L/',
                                   image.names = specimen.factors.lft[,1],
                                   nlandmarks = 12,
                                   writedir = 'data/',
                                   tps.filename = 'specimen_LM_lft.TPS')


specimen.factors.dor <- read.csv('specimen.csv', header = F)

specimen.LM.dor <- landmark.images(imagedir = 'images/standardised/standardised D/',
                                   image.names = specimen.factors.dor[,1],
                                   nlandmarks = 12,
                                   writedir = 'data/',
                                   tps.filename = 'specimen_LM_dor.TPS')


# RUN ONLY IF 'NA' appeared in the TPS file, otherwise jump to next step
'STEP:
1. If error occurs because of missing landmarks, check for NAs in TPS file
2. Run again only for the images with missing NAs
3. Input the value into the landmarks file'

specimen.factors.a.lft <- read.csv('add.csv', header = F)

specimen.LM.a.lft <- landmark.images(imagedir = 'images/standardised/standardised L/',
                                     image.names = specimen.factors.a.lft[,1],
                                     nlandmarks = 12,
                                     writedir = 'data/',
                                     tps.filename = 'addition_LM_lft.TPS')

'NOTE: row, column, id'
specimen.LM.a.lft[5, 2, 39] <- 1404.54370922647


specimen.factors.a.dor <- read.csv('add.csv', header = F)

specimen.LM.a.dor <- landmark.images(imagedir = 'images/standardised/standardised D/',
                                     image.names = specimen.factors.a.dor[,1],
                                     nlandmarks = 12,
                                     writedir = 'data/',
                                     tps.filename = 'addition_LM_dor.TPS')

'NOTE: row, column, id'
specimen.LM.a.dor[5, 2, 39] <- 1404.54370922647


# create unwarped images

specimen.LM.ext.lft <-  tps2array('data/specimen_LM_lft.TPS')
specimen.LM.ext.dor <-  tps2array('data/specimen_LM_dor.TPS')

perimeter.map.lft <- c(1, 4, 2, 5:7, 3, 8:12)
sliders.lft <- make.sliders(perimeter.map.lft, main.lms = 1:3)

unwarped.jpg.lft <- tps.unwarp(imagedir = 'images/standardised/standardised L/',
                               landmarks = specimen.LM.ext.lft,
                               image.names = specimen.factors.lft[,1],
                               sliders = sliders.lft,
                               write.dir = 'images/unwarped/unwarped L/')

perimeter.map.dor <- c(1, 4, 2, 5:8, 3, 9:12)
sliders.dor <- make.sliders(perimeter.map.dor, main.lms = 1:3)

unwarped.jpg.dor <- tps.unwarp(imagedir = 'images/standardised/standardised D/',
                               landmarks = specimen.LM.ext.dor,
                               image.names = specimen.factors.dor[,1],
                               sliders = sliders.dor,
                               write.dir = 'images/unwarped/unwarped D/')

'BEFORE PROCEEDING:
1. Before proceeding, make sure you convert the png output of unwarped images to jpg
2. Use the converted to jpg files'


### PRE-ANALYSIS - CREATING TARGET #####

# create landmark for only 1 image of choice, as target. Use the unwarped image.

specimen.factors.t.lft <- read.csv('target.csv', header = F)

specimen.LM.t.lft <- landmark.images(imagedir = 'images/specimen/specimen L/',
                                     image.names = specimen.factors.t.lft[,1],
                                     nlandmarks = 12,
                                     writedir = 'data/',
                                     tps.filename = 'target_LM_lft.TPS')

specimen.factors.t.dor <- read.csv('target.csv', header = F)

specimen.LM.t.dor <- landmark.images(imagedir = 'images/specimen/specimen D/',
                                     image.names = specimen.factors.t.dor[,1],
                                     nlandmarks = 12,
                                     writedir = 'data/',
                                     tps.filename = 'target_LM_dor.TPS')

'BEFORE PROCEEDING:
1. Use the target TPS file, then change it into specific format,
see example file, save it into .txt
2. Copy the .txt file for each image analysed'


### GENERATING DATA - COMMON CODES #####

# indicating sample id
#1 image failed to be standardised (smaller), excluded from lateral analysis (B043)
IDListFL <- c('B002','B010','B011','B012',
              'B013','B015','B019',
              'B033','B035','B037','B039',
              'B041','B042','B048',
              'B063',
              'B064','B072','B076','B078','B079','B080',
              'B081','B083','B084','B087',
              'B092','B094','B101')

IDListML <- c('B001','B003','B006','B008',
              'B009','B014','B020',
              'B016','B017','B018','B034',
              'B036','B040','B046',
              'B047',
              'B061','B062','B065',
              'B066','B067','B069','B071',
              'B073','B074','B075','B090',
              'B091','B082','B085','B086','B089',
              'B093','B096','B097','B098',
              'B099','B100','B102','B103')


#3 images failed to be standardised (smaller), excluded from dorsal analysis (B019. B061, B100)
IDListFD <- c('B002','B010','B011','B012',
              'B013','B015',
              'B033','B035','B037','B039',
              'B041','B042','B048',
              'B063',
              'B064','B072','B076','B078','B079','B080',
              'B081','B083','B084','B087',
              'B092','B094','B101')

IDListMD <- c('B001','B003','B006','B008',
              'B009','B014','B020',
              'B016','B017','B018','B034',
              'B036','B040','B043','B046',
              'B047',
              'B062','B065',
              'B066','B067','B069','B071',
              'B073','B074','B075','B090',
              'B091','B082','B085','B086','B089',
              'B093','B096','B097','B098',
              'B099','B102','B103')


# indicating sample images and landmarks

prepath <- 'landmarks/landmark L/'
extension <- '.txt'
landmarkListFL <- makeList(IDListFL, 'landmark', prepath, extension)
landmarkListML <- makeList(IDListML, 'landmark', prepath, extension)

prepath <- 'landmarks/landmark D/'
extension <- '.txt'
landmarkListFD <- makeList(IDListFD, 'landmark', prepath, extension)
landmarkListMD <- makeList(IDListMD, 'landmark', prepath, extension)


prepath <- 'images/specimen/specimen L/'
extension <- '_unwarped.jpg'
imageListFL <- makeList(IDListFL, 'image', prepath, extension)
imageListML <- makeList(IDListML, 'image', prepath, extension)

prepath <- 'images/specimen/specimen D/'
extension <- '_unwarped.jpg'
imageListFD <- makeList(IDListFD, 'image', prepath, extension)
imageListMD <- makeList(IDListMD, 'image', prepath, extension)


# indicating target

'STEP:
1. For outline, the target image that we used before,
open the unwarp image of it in FIJI
2. Use polygon tools
3. Create XY coordinates for it'

outline_lft <- read.table('landmarks/B037_outline_lft.txt', h= F)
target_lft <- imageListFL[['B037']]

outline_dor <- read.table('landmarks/B037_outline_dor.txt', h= F)
target_dor <- imageListFD[['B037']]



### GENERATING DATA - PATTERN EXTRACTION AND QUANTIFICATION #####

# extracting patterns

'STEP:
1. Each image will appear
2. You have to indicate which one is the pattern and which one is background
3. No definite number of how many you need to indicate,
   as much as you like and need, the more, the accurate
4. You will see what I mean after running it by yourself
5. You can redo the marking process, if you are unhappy with the extracted pattern'

rasterList_F_lft <- patLanW(imageListFL, landmarkListFL, IDListFL,
                            transformRef = 'B037',
                            resampleFactor = 5,
                            plotTransformed = TRUE,
                            correct = TRUE, plotCorrect = FALSE,
                            blur = FALSE, sigma = 2,
                            bucketfill = FALSE, cleanP = 3,
                            adjustCoords = TRUE,
                            splitC = 10, plotPriority = TRUE,
                            plotWS = TRUE, plotBF = TRUE,
                            plotFinal = TRUE, maskOutline = outline_lft, cartoonID = 'B037')

save(rasterList_F_lft, file = 'data/rasterList_F_lft.rda')
load('data/rasterList_F_lft.rda')

rasterList_M_lft <- patLanW(imageListML, landmarkListML, IDListML,
                            transformRef = 'B062',
                            resampleFactor = 5,
                            plotTransformed = TRUE,
                            correct = TRUE, plotCorrect = FALSE,
                            blur = FALSE, sigma = 2,
                            bucketfill = FALSE, cleanP = 3,
                            adjustCoords = TRUE,
                            splitC = 10, plotPriority = TRUE,
                            plotWS = TRUE, plotBF = TRUE,
                            plotFinal = TRUE, maskOutline = outline_lft, cartoonID = 'B062')

save(rasterList_M_lft, file = 'data/rasterList_M_lft.rda')
load('data/rasterList_M_lft.rda')

rasterList_F_dor <- patLanW(imageListFD, landmarkListFD, IDListFD,
                            transformRef = 'B037',
                            resampleFactor = 5,
                            plotTransformed = TRUE,
                            correct = TRUE, plotCorrect = FALSE,
                            blur = FALSE, sigma = 2,
                            bucketfill = FALSE, cleanP = 3,
                            adjustCoords = TRUE,
                            splitC = 10, plotPriority = TRUE,
                            plotWS = TRUE, plotBF = TRUE,
                            plotFinal = TRUE, maskOutline = outline_dor, cartoonID = 'B037')

save(rasterList_F_dor, file = 'data/rasterList_F_dor.rda')
load('data/rasterList_F_dor.rda')

rasterList_M_dor <- patLanW(imageListMD, landmarkListMD, IDListMD,
                            transformRef = 'B062',
                            resampleFactor = 5,
                            plotTransformed = TRUE,
                            correct = TRUE, plotCorrect = FALSE,
                            blur = FALSE, sigma = 2,
                            bucketfill = FALSE, cleanP = 3,
                            adjustCoords = TRUE,
                            splitC = 10, plotPriority = TRUE,
                            plotWS = TRUE, plotBF = TRUE,
                            plotFinal = TRUE, maskOutline = outline_dor, cartoonID = 'B062')

save(rasterList_M_dor, file = 'data/rasterList_M_dor.rda')
load('data/rasterList_M_dor.rda')


#rasterList_F_lft <- c(rasterList_FC, rasterList_FO)
#rasterList_M_lft <- c(rasterList_MC, rasterList_MO)
#save(rasterList_F_lft, file = 'data/rasterList_F_lft.rda')
#save(rasterList_M_lft, file = 'data/rasterList_M_lft.rda')

#rasterList_F_dor <- c(rasterList_FC_dor, rasterList_FO_dor)
#rasterList_M_dor <- c(rasterList_MC_dor, rasterList_MO_dor)
#save(rasterList_F_dor, file = 'data/rasterList_F_dor.rda')
#save(rasterList_M_dor, file = 'data/rasterList_M_dor.rda')

# if needed for editing raster list
#rasterList_M_lft <- c(rasterList_M_lft[-c(14)])


# sum the extracted patterns

summedRaster_F_lft <- sumRaster(rasterList_F_lft, IDListFL, type = 'RGB')
summedRaster_M_lft <- sumRaster(rasterList_M_lft, IDListML, type = 'RGB')

summedRaster_F_dor <- sumRaster(rasterList_F_dor, IDListFD, type = 'RGB')
summedRaster_M_dor <- sumRaster(rasterList_M_dor, IDListMD, type = 'RGB')


# plotting patterns

colfunc <- c('grey0','white','#63BCC9')
#colfunc <- inferno(100, end = 0.9, direction = 1)


png(file = 'Figure_1_FL.png',
    width = 10, height = 6, units = 'in', res = 1200)
plotHeat(summedRaster_F_lft, IDListFL, plotCartoon = FALSE, refShape = 'target', outline = outline_lft,
         adjustCoords = FALSE, crop = c(0,0,0,0), refImage = imageListFL[['B037']],
         imageList = imageListFL, cartoonID = 'B037', cartoonFill = 'black', cartoonOrder = 'under',
         colpalette = colfunc, format = 'imageJ', legendTitle = 'proportion of individuals with colour',
         legend.side = 4)
dev.off()

png(file = 'Figure_1_ML.png',
    width = 10, height = 6, units = 'in', res = 1200)
plotHeat(summedRaster_M_lft, IDListML, plotCartoon = FALSE, refShape = 'target', outline = outline_lft,
         adjustCoords = FALSE, crop = c(0,0,0,0), refImage = imageListFL[['B037']],
         imageList = imageListML, cartoonID = 'B037', cartoonFill = 'black', cartoonOrder = 'under',
         colpalette = colfunc, format = 'imageJ', legendTitle = 'proportion of individuals with colour',
         legend.side = 4)
dev.off()

png(file = 'Figure_1_FD.png',
    width = 10, height = 6, units = 'in', res = 1200)
plotHeat(summedRaster_F_dor, IDListFD, plotCartoon = FALSE, refShape = 'target', outline = outline_dor,
         adjustCoords = FALSE, crop = c(0,0,0,0), refImage = imageListFD[['B037']],
         imageList = imageListFD, cartoonID = 'B037', cartoonFill = 'black', cartoonOrder = 'under',
         colpalette = colfunc, format = 'imageJ', legendTitle = 'proportion of individuals with colour',
         legend.side = 4)
dev.off()

png(file = 'Figure_1_MD.png',
    width = 10, height = 6, units = 'in', res = 1200)
plotHeat(summedRaster_M_dor, IDListMD, plotCartoon = FALSE, refShape = 'target', outline = outline_dor,
         adjustCoords = FALSE, crop = c(0,0,0,0), refImage = imageListFD[['B037']],
         imageList = imageListMD, cartoonID = 'B037', cartoonFill = 'black', cartoonOrder = 'under',
         colpalette = colfunc, format = 'imageJ', legendTitle = 'proportion of individuals with colour',
         legend.side = 2)
dev.off()


# calculating pattern area proportion

area_F_lft <- patArea(rasterList_F_lft, IDListFL, refShape = 'target', type = 'RGB',
                      outline = outline_lft, crop = c(0,0,0,0), adjustCoords = TRUE,
                      imageList = imageListFL, cartoonID = 'B037')

area_M_lft <- patArea(rasterList_M_lft, IDListML, refShape = 'target', type = 'RGB',
                      outline = outline_lft, crop = c(0,0,0,0), adjustCoords = TRUE,
                      imageList = imageListML, cartoonID = 'B037')

area_F_dor <- patArea(rasterList_F_dor, IDListFD, refShape = 'target', type = 'RGB',
                      outline = outline_dor, crop = c(0,0,0,0), adjustCoords = TRUE,
                      imageList = imageListFD, cartoonID = 'B037')

area_M_dor <- patArea(rasterList_M_dor, IDListMD, refShape = 'target', type = 'RGB',
                      outline = outline_dor, crop = c(0,0,0,0), adjustCoords = TRUE,
                      imageList = imageListMD, cartoonID = 'B037')


area_F_lft$Sex <- 'fem'
area_F_dor$Sex <- 'fem'
area_M_lft$Sex <- 'mle'
area_M_dor$Sex <- 'mle'

area_F_lft$Side <- 'lateral'
area_F_dor$Side <- 'dorsal'
area_M_lft$Side <- 'lateral'
area_M_dor$Side <- 'dorsal'

area_all <- rbind(area_F_lft, area_M_lft, area_F_dor, area_M_dor)
write.csv(area_all, 'data/data_pattern_area.csv', row.names = F)
area_all <- read.csv('data/data_pattern_area.csv')

### VISUALISATION - PATTERN AREA PROPORTION #####

alpine.weevil <- c('#63BCC9', '#F9E2AE')
palette_plot(alpine.weevil)

plot.pap_lft <- ggbetweenstats(data = area_all[area_all$Side == 'lateral',], x = Sex, y = Area)
plot.pap_lft

plot.pap_dor <- ggbetweenstats(data = area_all[area_all$Side == 'dorsal',], x = Sex, y = Area)
plot.pap_dor

sex.labs <- c('fem' = '\u2640', 'mle' = '\u2642')

plot.pap <- ggplot(area_all, aes(x = Sex, y = Area, fill = Sex)) +
  geom_violin(width = 0.5, alpha = 0, linewidth = 1, colour = 'grey50') +
  geom_boxplot(width = 0.4, cex = 0.5, alpha = 0.1, linewidth = 1, colour = 'grey50', fill ='grey50') +
  geom_signif(comparisons = list(c('fem', 'mle')),
              map_signif_level = T, textsize = 6, tip_length = 0, vjust = 2) +
  geom_point(aes(colour = Sex), position = position_jitter(0), alpha = 0.4, size = 5, stroke = 0) +
  stat_summary(fun = mean, geom = 'point', shape = 16,
               size = 5, color = 'grey30') +
  facet_wrap(vars(Side)) +
  scale_y_continuous(bquote(Relative ~ pattern ~ area ~ (mm^2))) +
  scale_x_discrete(labels = (text = sex.labs)) +
  scale_color_manual(values = c('#63BCC9', '#F9E2AE')) +
  scale_fill_manual(values = c('#63BCC9', '#F9E2AE')) +
  xlab('\nSex') +
  theme_bw() +
  theme(legend.position = 'none',
        strip.background = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12))

plot.pap
ggsave(filename = 'Figure_1c.png', width = 5, height = 5, device = 'png', dpi = 1200)


### VISUALISATION - SIGNAL ALLOMETRY #####

allometry$lft.log <- log(allometry$lft)
allometry$dor.log <- log(allometry$dor)
allometry$el.log <- log(allometry$el)
allometry$ew.log <- log(allometry$ew)
allometry$eh.log <- log(allometry$eh)

allft <- read.csv('data/data_allometry_lateral.csv')
aldor <- read.csv('data/data_allometry_dorsal.csv')

allft$side <- as.factor(allft$side)
aldor$side <- as.factor(aldor$side)
allft$side <- factor(allft$side, levels = c('el', 'ew', 'eh'))
aldor$side <- factor(aldor$side, levels = c('el', 'ew', 'eh'))

side.labs <- c('elytral height', 'elytral length', 'elytral width')
names(side.labs) <- c('eh', 'el', 'ew')

# lateral
plot.allft <- ggplot(allft, aes(x = log(size), y = log(lft), fill = sex)) +
  geom_point(aes(colour = sex), alpha = 0.8, size = 5, stroke = 0) +
  geom_smooth(method = lm, se = TRUE, aes(colour = sex), fill = 'grey50', alpha = 0.2) +
  geom_xsidedensity(aes(y = after_stat(density), fill = sex, colour = sex), alpha = 0.5, size = 1) +
  geom_ysidedensity(aes(x = after_stat(density), fill = sex, colour = sex), alpha = 0.5, size = 1) +
  scale_color_manual(values = c('#63BCC9', '#F9E2AE')) +
  scale_fill_manual(values = c('#63BCC9', '#F9E2AE')) +
  xlab('\nlog body size (mm)') +
  ylab(bquote(log ~ relative ~ lateral ~ pattern ~ area ~ (mm^2))) +
  facet_wrap(vars(side), scales = 'free', labeller = labeller(side = side.labs)) +
  theme_bw() +
  theme(legend.position = c(0.97, 0), legend.justification = c(1, 0),
        legend.key = element_blank(),
        legend.background = element_blank(),
        strip.background = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 16))

plot.allft
ggsave(filename = 'Figure_2a.1.png', width = 15, height = 5.5, device = 'png', dpi = 1200)


plot.allft <- ggplot(allft, aes(x = size, y = lft, fill = sex)) +
  geom_point(aes(colour = sex), alpha = 0.8, size = 5, stroke = 0) +
  geom_smooth(method = lm, se = TRUE, aes(colour = sex), fill = 'grey50', alpha = 0.2) +
  scale_color_manual(values = c('#63BCC9', '#F9E2AE')) +
  scale_fill_manual(values = c('#63BCC9', '#F9E2AE')) +
  xlab('\nBody size (mm)') +
  ylab(bquote(Relative ~ lateral ~ pattern ~ area ~ (mm^2))) +
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log', limits = c(0.012, 0.12),  breaks = function(y) seq(floor(min(y, digits = 1)),
                                                                                       ceiling(max(y, digits = 1)),
                                                                                       by = 0.02)) +
  facet_wrap(vars(side), scales = 'free', labeller = labeller(side = side.labs)) +
  theme_bw() +
  theme(legend.position = c(0.97, 0), legend.justification = c(1, 0),
        legend.key = element_blank(),
        legend.background = element_blank(),
        strip.background = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 16))

plot.allft
ggsave(filename = 'Figure_2a.2.png', width = 15, height = 5.5, device = 'png', dpi = 1200)


# dorsal
plot.aldor <- ggplot(aldor, aes(x = log(size), y = log(dor), fill = sex)) +
  geom_point(aes(colour = sex), alpha = 0.8, size = 5, stroke = 0) +
  geom_smooth(method = lm, se = TRUE, aes(colour = sex), fill = 'grey50', alpha = 0.2) +
  geom_xsidedensity(aes(y = after_stat(density), fill = sex, colour = sex), alpha = 0.5, size = 1) +
  geom_ysidedensity(aes(x = after_stat(density), fill = sex, colour = sex), alpha = 0.5, size = 1) +
  scale_color_manual(values = c('#63BCC9', '#F9E2AE')) +
  scale_fill_manual(values = c('#63BCC9', '#F9E2AE')) +
  xlab('\nlog body size (mm)') +
  ylab(bquote(log ~ relative ~ dorsal ~ pattern ~ area ~ (mm^2))) +
  facet_wrap(vars(side), scales = 'free', labeller = labeller(side = side.labs)) +
  theme_bw() +
  theme(legend.position = c(0.97, 0), legend.justification = c(1, 0),
        legend.key = element_blank(),
        legend.background = element_blank(),
        strip.background = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 16))

plot.aldor
ggsave(filename = 'Figure_2b.1.png', width = 15, height = 5.5, device = 'png', dpi = 1200)


plot.aldor <- ggplot(aldor, aes(x = size, y = dor, fill = sex)) +
  geom_point(aes(colour = sex), alpha = 0.8, size = 5, stroke = 0) +
  geom_smooth(method = lm, se = TRUE, aes(colour = sex), fill = 'grey50', alpha = 0.2) +
  scale_color_manual(values = c('#63BCC9', '#F9E2AE')) +
  scale_fill_manual(values = c('#63BCC9', '#F9E2AE')) +
  xlab('\nBody size (mm)') +
  ylab(bquote(Relative ~ dorsal ~ pattern ~ area ~ (mm^2))) +
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log', limits = c(0.01,0.089),  breaks = function(y) seq(floor(min(y, digits = 1)),
                                                                                      ceiling(max(y, digits = 1)),
                                                                                      by = 0.02)) +
  facet_wrap(vars(side), scales = 'free', labeller = labeller(side = side.labs)) +
  theme_bw() +
  theme(legend.position = c(0.97, 0), legend.justification = c(1, 0),
        legend.key = element_blank(),
        legend.background = element_blank(),
        strip.background = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 16))

plot.aldor
ggsave(filename = 'Figure_2b.2.png', width = 15, height = 5.5, device = 'png', dpi = 1200)


### VISUALISATION - SUPPLEMENTARY #####

# paired plot of dorsal and lateral pattern area
plot.pap_fem <- ggbetweenstats(data = area_all[area_all$Sex == 'fem',], x = Side, y = Area)
plot.pap_fem

plot.pap_mle <- ggbetweenstats(data = area_all[area_all$Sex == 'mle',], x = Side, y = Area)
plot.pap_mle


sex.labs <- c('\u2640', '\u2642')
names(sex.labs) <- c('fem', 'mle')

plot.pscor <- ggplot(area_all, aes(x = Side, y = Area, fill = Sex)) +
  geom_violin(width = 0.5, linewidth = 1, alpha = 0, colour = 'grey50') +
  geom_boxplot(width = 0.4, cex = 0.5, alpha = 0.1, linewidth = 1, colour = 'grey50', fill = 'grey50') +
  geom_line(aes(group = SampleId, colour = Sex), linewidth = 1, alpha = 0.5) +
  geom_point(aes(colour = Sex), position = position_jitter(0), alpha = 0.4, size = 5, stroke = 0) +
  stat_summary(fun = mean, geom = 'point', shape = 16,
               size = 5, color = 'grey30') +
  facet_wrap(vars(Sex), labeller = labeller(Sex = sex.labs)) +
  scale_y_continuous('Relative pattern area\n') +
  scale_color_manual(values = c('#63BCC9', '#F9E2AE')) +
  scale_fill_manual(values = c('#63BCC9', '#F9E2AE')) +
  xlab('\nSide') +
  theme_bw() +
  theme(legend.position = 'none',
        strip.background = element_blank(),
        axis.text.x = element_text(size = 12),
        strip.text = element_text(size = 12))

plot.pscor


# original value correlation, altitude effects, and sexual size dimorphism
allometry <- read.csv('data/data_allometry.csv')

plot.pear <- ggpairs(allometry,
                     columns = c('altitude', 'elytral.length', 'elytral.width', 'elytral.height',
                                 'lateral', 'dorsal', 'sex'),
                     upper = list(continuous = 'cor'),
                     lower = list(continuous = 'smooth'),
                     mapping = ggplot2::aes(colour = sex, alpha = 0.5))

for(i in 1:7) {
  for(j in 1:7){
    plot.pear[i,j] <- plot.pear[i,j] +
      scale_color_manual(values = c('#63BCC9', '#F9E2AE')) +
      scale_fill_manual(values = c('#63BCC9', '#F9E2AE'))
  }
}

plot.pear
ggsave(filename = 'Figure_S3.png', width = 10, height = 8, device = 'png', dpi = 1200)


### ANALYSIS - SEXUAL PATTERN DIMORPHISM #####

qqPlot(allometry$lateral)
qqPlot(allometry$dorsal)

# difference between sex
wilcox.test(lateral ~ sex, data = allometry)
wilcox.test(dorsal ~ sex, data = allometry)

# difference between side
wilcox.test(Area ~ Side, data = area_all[area_all$Sex == 'fem', ])
wilcox.test(Area ~ Side, data = area_all[area_all$Sex == 'mle', ])


### ANALYSIS - SIGNAL SIZE ALLOMETRY ####

ttest <- function(reg, coefnum, val){
  co <- coef(summary(reg))
  tstat <- (co[coefnum,1]-val)/co[coefnum,2]
  2 * pt(abs(tstat), reg$df.residual, lower.tail = FALSE)
}

# lateral
lm.lft.el.fem <- lm(log(lft) ~ log(size), data = allft, subset = side=="el" & sex=="fem")
lm.lft.el.fem
summary(lm.lft.el.fem)
ttest(lm.lft.el.fem, 2, 1)
simulateResiduals(lm.lft.el.fem, plot = TRUE)

lm.lft.el.mle <- lm(log(lft) ~ log(size), data = allft, subset = side=="el" & sex=="mle")
lm.lft.el.mle
summary(lm.lft.el.mle)
ttest(lm.lft.el.mle, 2, 1)
simulateResiduals(lm.lft.el.mle, plot = TRUE)

lm.lft.ew.fem <- lm(log(lft) ~ log(size), data = allft, subset = side=="ew" & sex=="fem")
lm.lft.ew.fem
summary(lm.lft.ew.fem)
ttest(lm.lft.ew.fem, 2, 1)
simulateResiduals(lm.lft.ew.fem, plot = TRUE)

lm.lft.ew.mle <- lm(log(lft) ~ log(size), data = allft, subset = side=="ew" & sex=="mle")
lm.lft.ew.mle
summary(lm.lft.ew.mle)
ttest(lm.lft.ew.mle, 2, 1)
simulateResiduals(lm.lft.ew.mle, plot = TRUE)

lm.lft.eh.fem <- lm(log(lft) ~ log(size), data = allft, subset = side=="eh" & sex=="fem")
lm.lft.eh.fem
summary(lm.lft.eh.fem)
ttest(lm.lft.eh.fem, 2, 1)
simulateResiduals(lm.lft.eh.fem, plot = TRUE)

lm.lft.eh.mle <- lm(log(lft) ~ log(size), data = allft, subset = side=="eh" & sex=="mle")
lm.lft.eh.mle
summary(lm.lft.eh.mle)
ttest(lm.lft.eh.mle, 2, 1)
simulateResiduals(lm.lft.eh.mle, plot = TRUE)


# dorsal
lm.dor.el.fem <- lm(log(dor) ~ log(size), data = aldor, subset = side=="el" & sex=="fem")
lm.dor.el.fem
summary(lm.dor.el.fem)
ttest(lm.dor.el.fem, 2, 1)
simulateResiduals(lm.dor.el.fem, plot = TRUE)

lm.dor.el.mle <- lm(log(dor) ~ log(size), data = aldor, subset = side=="el" & sex=="mle")
lm.dor.el.mle
summary(lm.dor.el.mle)
ttest(lm.dor.el.mle, 2, 1)
simulateResiduals(lm.dor.el.mle, plot = TRUE)

lm.dor.ew.fem <- lm(log(dor) ~ log(size), data = aldor, subset = side=="ew" & sex=="fem")
lm.dor.ew.fem
summary(lm.dor.ew.fem)
ttest(lm.dor.ew.fem, 2, 1)
simulateResiduals(lm.dor.ew.fem, plot = TRUE)

lm.dor.ew.mle <- lm(log(dor) ~ log(size), data = aldor, subset = side=="ew" & sex=="mle")
lm.dor.ew.mle
summary(lm.dor.ew.mle)
ttest(lm.dor.ew.mle, 2, 1)
simulateResiduals(lm.dor.ew.mle, plot = TRUE)

lm.dor.eh.fem <- lm(log(dor) ~ log(size), data = aldor, subset = side=="eh" & sex=="fem")
lm.dor.eh.fem
summary(lm.dor.eh.fem)
ttest(lm.dor.eh.fem, 2, 1)
simulateResiduals(lm.dor.eh.fem, plot = TRUE)

lm.dor.eh.mle <- lm(log(dor) ~ log(size), data = aldor, subset = side=="eh" & sex=="mle")
lm.dor.eh.mle
summary(lm.dor.eh.mle)
ttest(lm.dor.eh.mle, 2, 1)
simulateResiduals(lm.dor.eh.mle, plot = TRUE)


# significance between sex, lateral
lm.lft.el <- lm(log(lft) ~ log(size) * sex, data = allft, subset = side=='el')
lm.lft.el
summary(lm.lft.el)
Anova(lm.lft.el)
simulateResiduals(lm.lft.el, plot = TRUE)

lm.lft.ew <- lm(log(lft) ~ log(size) * sex, data = allft, subset = side=='ew')
lm.lft.ew
summary(lm.lft.ew)
Anova(lm.lft.ew)
simulateResiduals(lm.lft.ew, plot = TRUE)

lm.lft.eh <- lm(log(lft) ~ log(size) * sex, data = allft, subset = side=='eh')
lm.lft.eh
summary(lm.lft.eh)
Anova(lm.lft.eh)
simulateResiduals(lm.lft.eh, plot = TRUE)


# significance between sex, dorsal
lm.dor.el <- lm(log(dor) ~ log(size) * sex, data = aldor, subset = side=='el')
lm.dor.el
summary(lm.dor.el)
Anova(lm.dor.el)
simulateResiduals(lm.dor.el, plot = TRUE)

lm.dor.ew <- lm(log(dor) ~ log(size) * sex, data = aldor, subset = side=='ew')
lm.dor.ew
summary(lm.dor.ew)
Anova(lm.dor.ew)
simulateResiduals(lm.dor.ew, plot = TRUE)

lm.dor.eh <- lm(log(dor) ~ log(size) * sex, data = aldor, subset = side=='eh')
lm.dor.eh
summary(lm.dor.eh)
Anova(lm.dor.eh)
simulateResiduals(lm.dor.eh, plot = TRUE)


### ANALYSIS - SUPPLEMENTARY #####

# sexual size dimorphism
qqPlot(allometry$elytral.length)
qqPlot(allometry$elytral.width)
qqPlot(allometry$elytral.height)

allometry.ttest <- allometry %>%
  gather(key = "variable", value = "value", elytral.length, elytral.width, elytral.height) %>%
  group_by(variable) %>%
  t_test(value ~ sex) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance()

allometry.ttest

wilcox.test(elytral.length ~ sex, data = allometry)
wilcox.test(elytral.width ~ sex, data = allometry)
wilcox.test(elytral.height ~ sex, data = allometry)

