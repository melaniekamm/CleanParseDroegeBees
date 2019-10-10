#manually re-assign sampling information to correct column

a <- filter(mdde, TrapVolume == 'blue') %>%
            mutate(TrapLiquid = TrapColor, TrapColor=TrapVolume, TrapVolume='UNK')
   
b <- filter(mdde, TrapVolume == 'blue-uv') %>%
     mutate(TrapLiquid = TrapColor, TrapColor=TrapVolume, TrapVolume=NTraps)

c <- filter(b, TrapVolume == 8) %>%
      mutate(TrapVolume = 'UNK')
d <- filter(b, NTraps == 'bowl 3.25oz') %>%
     mutate(NTraps = 'UNK')
b <- rbind(c, d)

e <- filter(mdde, TrapVolume== 'glycol propylene') %>%
  mutate(TrapLiquid = TrapVolume, TrapVolume='UNK')

f <- filter(mdde, TrapVolume == 'pale blue-uv') %>%
  mutate(TrapLiquid = TrapColor, TrapColor=TrapVolume, TrapVolume='UNK')

g <- filter(mdde, TrapVolume == 'soap dawn' & is.na(TrapLiquid)) %>%
  mutate(TrapLiquid= 'soap dawn', TrapVolume= NA) 

h <- filter(mdde, TrapVolume == 'white') %>%
  mutate(TrapLiquid = TrapColor, TrapColor=TrapVolume, TrapVolume=NTraps)

i <- filter(mdde, TrapVolume == 'white-uv') %>%
  mutate(TrapLiquid = TrapColor, TrapColor=TrapVolume, TrapVolume='UNK')

j <- filter(mdde, TrapVolume == 'yellow') %>%
  mutate(TrapLiquid = TrapColor, TrapColor=TrapVolume, TrapVolume='UNK')

k <- filter(mdde, TrapVolume == 'yellow-uv') %>%
  mutate(TrapLiquid = TrapColor, TrapColor=TrapVolume, TrapVolume=NTraps, NTraps=NA)

fix <- rbind(a, b, e, f, g, h, i , j, k)

l <- filter(fix, TrapVolume == 8) %>%
      mutate(NTraps= TrapVolume, TrapVolume = 'UNK')

fix <- filter(fix, !identifier %in% l$identifier) %>%
  rbind(l)

mdde <- filter(mdde, !identifier %in% fix$identifier) %>%
    rbind(fix)

table(mdde$TrapColor)

a <- filter(mdde, TrapColor == 'ammonia') %>%
     mutate(TrapLiquid=TrapColor, TrapColor='UNK')

b <- filter(mdde, TrapColor == 'glycol propylene') %>%
     mutate(TrapLiquid=TrapColor, TrapColor='in field note')

c <- filter(mdde, TrapColor == 'soap dawn') %>%
  mutate(TrapLiquid=TrapColor, TrapColor='in field note')

d <- filter(c, !grepl(TrapVolume, pattern="bowl")) %>%
     mutate(TrapVolume=NTraps, NTraps='in field note')

c <- filter(c, !identifier %in% d$identifier) %>%
  rbind(d)

d <- filter(mdde, TrapColor == 'soap lab') %>%
  mutate(TrapLiquid=TrapColor, TrapColor='UNK')

fix2 <- rbind(a, b, c, d)

mdde <- filter(mdde, !identifier %in% fix2$identifier) %>%
  rbind(fix2)

e <- filter(mdde, TrapVolume==15) %>%
      mutate(NTraps=TrapVolume, TrapVolume ='bowl 3.25oz and bowl 16 oz')

f <- filter(mdde, TrapVolume==6) %>%
  mutate(NTraps=TrapVolume, TrapVolume ='UNK')

fix3 <- rbind(e, f)
mdde <- filter(mdde, !identifier %in% fix3$identifier) %>%
  rbind(fix3)

table(mdde$TrapVolume)
table(mdde$TrapColor)
table(mdde$TrapLiquid)
table(mdde$NTraps)

a <- filter(mdde, NTraps %in% c('bowl 0.75oz', 'bowl 12.0oz', 'bowl 2.0oz', 'bowl 3.25oz')) %>%
     mutate(TrapVolume=NTraps, NTraps=NA)
b <- filter(mdde, NTraps == 'soap dawn') %>%
     mutate(TrapLiquid=NTraps, NTraps=NA)

fix4 <- rbind(a,b)
mdde <- filter(mdde, !identifier %in% fix4$identifier) %>%
  rbind(fix4)

rm(a, b, c, d, e, f, g, h, i, j, k, l); rm(fix, fix2, fix3, fix4)
