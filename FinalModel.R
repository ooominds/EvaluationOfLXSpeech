
require(mgcv)
require(itsadug)
require(ggplot2)
require(grid)

options(show.signif.stars=FALSE)

#################
### LOAD DATA ###
#################

load('modelDat.rda')

############
### GAMM ###
############

summary(final.model <- gam(AcceptabilityScore ~
    SpeakerGender +
    Comprehensibility +
    Grammaticality * Accent +
    s(Conscientiousness, by=Accent, k=4) +
    s(Extraversion, by=Accent, k=4) +
    s(Agreeableness, by=Accent, k=4) +
    s(TrialOrder, ParticipantID, bs='fs', m=1) +
    s(Block, bs='re'),
    data=modelDat,
    method='ML'))
# Parametric coefficients:
#                         Estimate Std. Error t value Pr(>|t|)
# (Intercept)              0.80826    0.09592   8.426  < 2e-16
# SpeakerGenderM          -0.22228    0.03184  -6.981 4.13e-12
# Comprehensibilitysubmax -0.32202    0.05000  -6.441 1.53e-10
# Grammaticality1         -0.61542    0.04286 -14.358  < 2e-16
# AccentP                 -0.30136    0.04780  -6.304 3.65e-10
# Grammaticality1:AccentP  0.20262    0.05758   3.519 0.000444
# 
# Approximate significance of smooth terms:
#                                  edf  Ref.df      F  p-value
# s(Conscientiousness):AccentB   1.000   1.000  0.959   0.3276
# s(Conscientiousness):AccentP   1.000   1.000  4.601   0.0321
# s(Extraversion):AccentB        1.001   1.001  0.872   0.3502
# s(Extraversion):AccentP        2.893   2.981 13.816 3.19e-07
# s(Agreeableness):AccentB       1.000   1.001  0.410   0.5220
# s(Agreeableness):AccentP       2.294   2.622  4.321   0.0328
# s(TrialOrder,ParticipantID)  168.333 533.000  3.389  < 2e-16
# s(Block)                       6.011   7.000  7.460  < 2e-16
# 
# R-sq.(adj) =    0.6   Deviance explained = 63.8%
# -ML = 2035.7  Scale est. = 0.37802   n = 1971

summary(final.model.t <- gam(AcceptabilityScore ~
    SpeakerGender +
    Comprehensibility +
    Grammaticality * Accent +
    s(Conscientiousness, by=Accent, k=4) +
    s(Extraversion, by=Accent, k=4) +
    s(Agreeableness, by=Accent, k=4) +
    s(TrialOrder, ParticipantID, bs='fs', m=1) +
    s(Block, bs='re'),
    data=modelDat,
    subset=abs(scale(resid(final.model)))<2.5,
    method='ML'))
# Parametric coefficients:
#                         Estimate Std. Error t value Pr(>|t|)
# (Intercept)              0.84998    0.09495   8.952  < 2e-16
# SpeakerGenderM          -0.22004    0.03000  -7.336 3.40e-13
# Comprehensibilitysubmax -0.35883    0.04688  -7.655 3.22e-14
# Grammaticality1         -0.64814    0.03857 -16.804  < 2e-16
# AccentP                 -0.33769    0.04345  -7.771 1.33e-14
# Grammaticality1:AccentP  0.24794    0.05167   4.798 1.74e-06
# 
# Approximate significance of smooth terms:
#                                  edf  Ref.df      F p-value
# s(Conscientiousness):AccentB   1.000   1.000  0.852  0.3559
# s(Conscientiousness):AccentP   1.000   1.000  4.806  0.0285
# s(Extraversion):AccentB        1.000   1.000  0.722  0.3955
# s(Extraversion):AccentP        2.896   2.981 16.057  <2e-16
# s(Agreeableness):AccentB       1.000   1.000  0.457  0.4991
# s(Agreeableness):AccentP       2.376   2.685  5.061  0.0161
# s(TrialOrder,ParticipantID)  200.880 533.000  4.495  <2e-16
# s(Block)                       6.033   7.000  8.322  <2e-16
# 
# R-sq.(adj) =  0.671   Deviance explained = 70.9%
# -ML = 1791.5  Scale est. = 0.29293   n = 1929

#############
### PLOTS ###
#############

genderdat = get_predictions(final.model.t,
    cond=list(SpeakerGender=c('F', 'M')))
genderdat$lo95 = genderdat$fit - genderdat$CI
genderdat$hi95 = genderdat$fit + genderdat$CI
genderdat$SpeakerGender = relevel(genderdat$SpeakerGender, ref='F')

comprehendat = get_predictions(final.model.t,
    cond=list(Comprehensibility=c('submax', 'max')))
comprehendat$lo95 = comprehendat$fit - comprehendat$CI
comprehendat$hi95 = comprehendat$fit + comprehendat$CI
comprehendat$Comprehensibility = relevel(comprehendat$Comprehensibility, ref='submax')

errordat = get_predictions(final.model.t,
    cond=list(Grammaticality=c('0', '1'), Accent=c('B', 'P')))
errordat$lo95 = errordat$fit - errordat$CI
errordat$hi95 = errordat$fit + errordat$CI
errordat$Grammaticality = relevel(errordat$Grammaticality, ref='0')
errordat$Accent = relevel(errordat$Accent, ref='B')

p1 <- ggplot(genderdat, aes(x=SpeakerGender, y=fit)) +
    coord_cartesian(ylim=c(-0.6,0.6)) +
    geom_point(colour='#333333', position=position_dodge(.9), size=3, stat='identity') +
    geom_errorbar(colour='#333333', position=position_dodge(.9), width=.15, linewidth=.8,
        aes(ymin=lo95, ymax=hi95)) +
    scale_x_discrete(name='Gender of speaker',
        breaks=c('F', 'M'),
        labels=c('female', 'male')) +
    scale_y_continuous(name='Rating (transformed, scaled)') +
    theme(axis.text=element_text(size=10),
        axis.title.x=element_text(size=13),
        axis.title.y=element_text(size=13))

p2 <- ggplot(comprehendat, aes(x=Comprehensibility, y=fit)) +
    coord_cartesian(ylim=c(-0.6,0.6)) +
    geom_point(colour='#333333', position=position_dodge(.9), size=3, stat='identity') +
    geom_errorbar(colour='#333333', position=position_dodge(.9), width=.15, linewidth=.8,
        aes(ymin=lo95, ymax=hi95)) +
    scale_x_discrete(name='Comprehensibility',
        breaks=c('submax', 'max'),
        labels=c('less than maximal', 'maximal')) +
    scale_y_continuous(name=' ') +
    theme(axis.text=element_text(size=10),
        axis.title.x=element_text(size=13),
        axis.title.y=element_text(size=13))

p3 <- ggplot(errordat, aes(x=Accent, y=fit, colour=Grammaticality)) +
    coord_cartesian(ylim=c(-0.6,0.6)) +
    geom_point(position=position_dodge(.9), size=3, stat='identity') +
    geom_errorbar(position=position_dodge(.9), width=.15, linewidth=.8,
        aes(ymin=lo95, ymax=hi95)) +
    scale_x_discrete(name='Accent',
        breaks=c('B', 'P'),
        labels=c('British', 'Polish')) +
    scale_y_continuous(name=' ') +
    scale_colour_manual(name='Error:', values=c('#e41a1c', '#377eb8'),
        breaks=c('0', '1'),
        labels=c('not filled', 'filled')) +
    theme(axis.text=element_text(size=10),
        axis.title.x=element_text(size=13),
        axis.title.y=element_text(size=13))

pushViewport(viewport(
    layout=grid.layout(1, 3, heights=unit(5,'null'),
        widths=unit(c(4.3,4.3,5.4),'null'))))
print(p1, vp=viewport(layout.pos.row=1, layout.pos.col=1))
print(p2, vp=viewport(layout.pos.row=1, layout.pos.col=2))
print(p3, vp=viewport(layout.pos.row=1, layout.pos.col=3))

par(mfrow=c(1,3), mar=c(5,5,1,1))
plot_smooth(final.model.t, view='Conscientiousness',
    cond=list(Accent='P'), rm.ranef=TRUE, rug=FALSE, hide.label=TRUE,
    ylim=c(-1.5,1.2), xlab='Big Five: Conscientiousness', ylab='Effect')
plot_smooth(final.model.t, view='Extraversion',
    cond=list(Accent='P'), rm.ranef=TRUE, rug=FALSE, hide.label=TRUE,
    ylim=c(-1.5,1.2), xlab='Big Five: Extraversion', ylab=' ')
plot_smooth(final.model.t, view='Agreeableness',
    cond=list(Accent='P'), rm.ranef=TRUE, rug=FALSE, hide.label=TRUE,
    ylim=c(-1.5,1.2), xlab='Big Five: Agreeableness', ylab=' ')


