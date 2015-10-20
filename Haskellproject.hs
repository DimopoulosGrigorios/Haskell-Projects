binary n =if (n==1) then [[0],[1]] else [x:y|x<-[0,1],y<-binary (n-1)]
allbin = [binary x|x<-[1..]]

-----------------------------------------------------------------------------------------------------------------------------------
--Ylopoihsh ths member na epistrefei thn prwth thesh pou briskei to stoixeio  analoga me ton n pou tha parei
member n x [] = -1
member n x (y:xs) = if (x==y) then n
else (member (n+1) x xs)

select (-1) (x:xs)=(-1)
select 1 (x:xs) = x
select n (x:xs) = select (n-1) xs
--epistrofh listas me ta kanalia pou misoun ta paidia
check_the_hate []=[]
check_the_hate ((x,y):rest)= (y:check_the_hate rest)
--epistrofh listas me ta kanalia pou agapane ta paidia
check_the_like []=[]
check_the_like ((x,y):rest)= (x:check_the_like rest)
--Epistrofh ths allaghs pou tha prokipsei me thn lista ls kai to twrino kanali i
check i ls = select (member 1 i (check_the_hate ls)) (check_the_like ls)


allhappy2 k (-1) m i ls = -1
allhappy2 k n m i ls = if (i== -1) then (k-1) else allhappy2 (k+1) (n-1) (m-1) (check i ls) ls

allhappy n m i ls= allhappy2 0 n m i ls
------------------------------------------------------------------------------------------------------------------------------------
aces 0=[]
aces n = 1:aces (n-1)

zeros 0=[]
zeros n = 0:zeros (n-1)
--synarthsh pou sbhnei ta mhdenika
drop_zeros (x:xs) = if (x==0) then drop_zeros xs else (x:xs)

bits2 [] [] = []
bits2 [0] [1] =[1]
bits2 (1:frest) (1:srest)=(1:bits2 frest srest)
bits2 (0:frest) (0:srest)=(0:bits2 frest srest)
bits2 (0:frest) (1:srest)=(0:aces (length frest))



mostones2 fl sl =if (length sl ==sum sl) then sl else  bits2 (concat[zeros (length sl - length fl),fl] ) (sl)

mostones fl sl = drop_zeros (mostones2 fl sl)
----------------------------------------------------------------------------------------------------------------------------------------
member2 x [] = False
member2 x (y:xs) = if (x==y) then True
else (member2 x xs)
--Ylopoihsh dyo synarthsewn gia eksereunish twn listwn kai epistrofh toy megistoy monopatiou
--Dynatothta metapidishs apo thn mia synarthsh sthn allh
doublechain2 [] sl =0
doublechain2 (fe:fl) sl =  if (member2 fe sl) then (fe+max (doublechain2 fl sl ) (doublechain3 fl (drop (member 1 fe sl) sl))) else ((doublechain2 fl sl )+fe) 

doublechain3 fl [] =0
doublechain3 fl  (se:sl) = if (member2 se fl) then (se+max (doublechain3 fl sl ) (doublechain2 (drop (member 1 se fl) fl) sl  )) else   ((doublechain3 fl sl )+se)

doublechain fl sl= max (doublechain2 fl sl ) (doublechain3 fl sl )
----------------------------------------------------------------------------------------------------------------------------------------
--Synarthsh pou kanei lista apo arithmous mia lista apo (x,y)
make_it_list []=[]
make_it_list ((x,y):rest) = (x:y:make_it_list rest)

delete x [] = []
delete x (y:xs) = if (x==y) then (delete x xs)
else (y:delete x xs)

remove x list = concat [take (member 0 x list) list ,drop (member 1 x list) list]
--Synarthsh pou briskei posa idia uparxoun apo kapoio stoixeio se mia lista gia ola ta diaforetika stoixeia ths listas kai bgazei to apotelesma se mia lista
found_same []=[]
found_same (x:list)= ((length (x:list) - length (delete x (x:list))):found_same (delete x (x:list)))
--Synarth pou tsekarei an duo listes einai idies dhladh an periexoun ta idia stoixeia se opoiadhpote seira
same [] []=True
same [] slist=False
same list []=False
same (x:list) slist=if (member2 x slist) then same list (remove x slist) else False 

isomorphic flist slist= same (found_same(make_it_list flist)) (found_same(make_it_list slist))
