pico-8 cartridge // http://www.pico-8.com
version 27
__lua__
rates={}
reverb={}
distorsion={}
filter={}
ch1={}
ch2={}
ch3={}
ch4={}
ti={0,0,0,0,0,0}

-- these will contain values
-- for their channels, and
-- can be used for more exact
-- syncing, for example:
-- hit[1]=(sefx[1]&0b0011)
-- and so whenever there's a
-- bd hit (fx 3) on track 1,
-- hit[1] will be 1
note={0,0,0,0}
inst={0,0,0,0}
sefx={0,0,0,0}
volu={0,0,0,0}
hit={0,0,0,0,0}
hitf={0,0,0,0,0}


-- which effect is played in ptr
efu={2,2,2,2,2,2,2,2,
     3,0,0,0,0,0,0,0,
     3,0,0,0,0,0,0,0,
     --1,1,1,1,1,1,1,1,
     3,0,0,0,0,0,0,0,
     4,1,1,1,1,1,1,1,
     3,0,0,0,0,0,0,0}
     
-- select by using efu[stat(24)]
-- and show scene based on that     
     
-- control values for effects
efv={0b00000000, -- efu 0
     0b00001011, -- efu 1
     0b11111111} -- efu 2? more?
     
bg=0
scrstart=0x6000
scrend=0x7fff
t=0
tt=.0
dt=.0
ot=.0
xoff=0
yoff=0
cot=0
sit=0
of=0
f=0
b=0
c=0
p=0
bb=0
dd=0
sn_hit=0
bd2=0
dis=0
dis_f=0
rnd_efu=0
rec=-1


function _init()

 -- create a polyline
 rand_poly(-100,-100,50,100,100,250)
 --
 -- palette gradient
 poke4(0x5f10,0x8582.8000)
 poke4(0x5f14,0x0706.8605)
 poke4(0x5f18,0x088e.0987)
 poke4(0x5f1c,0x8084.0288)
 poke(0x5f2e,1)
 --]]
 
	-- music init
 poke(0x5f40,0b1111) --speed
	poke(0x5f41,0b0000) --reverb
	poke(0x5f42,0b0000) --dist
	poke(0x5f43,0b0001) --lowpass
	music(0)

end
 
snc=nil

function _update60()
 ot=tt
 tt=time()--*(0.05+sn_hit*0.028)
 
 snc=cocreate(sync)
	
 if snc and costatus(snc)!= 'dead' then
  coresume(snc)
 else
  snc=nil
 end 
 
 -- this is where you'd update
 -- values basaed on the current
 -- pattern and efu
 if efu[stat(24)+1]%2==0 then
  update_poly(hitf)
  --prot.z=prot.z+(sefx[1]&0b0001)*4
  prot.y=prot.y+0.01*(hitf[3]-1)*0.4
  rotate_poly(prot.x,prot.y,prot.z)
 else
 	stop_poly()
 end
 
 dt=time()-tt
end

--sync keeps track of music
--and values related to it
--(also some redundant/deprec.
-- things)
function sync()
 cot=cos(f*0.004)
 sit=sin(f*0.004)
 xoff=sin(tt*0.02)*20
 yoff=sin(tt*0.03)*22
 b=15--rnd_efu-btn()
 bb=stat(26)/3.
 pattern=stat(24)
 ch1=get_note(0)
 ch2=get_note(1)
 ch3=get_note(2)
 ch4=get_note(3)
 note={ch1[1],ch2[1],ch3[1],ch4[1]}
 inst={ch1[2],ch2[2],ch3[2],ch4[2]}
 sefx={ch1[3],ch2[3],ch3[3],ch4[3]}
 volu={ch1[4],ch2[4],ch3[4],ch4[4]}
 if note[2]==36 and inst[2]==6 and ti[2]+0.1>tt then
  hitf[1]=5
  ti[2]=tt
  dis_f=0
  
 else
  dis_f=dis_f*0.8
  dis=flr(dis_f)
  hitf[1]=hitf[1]*0.5
 end
 for i=1,8 do
  hit[i]=flr(hitf[i])
 end
 
 -- it seems the best way to
 -- get reliable results is
 -- to get the values to hit{}
 -- and then based on that,
 -- have hitf{} contain a float
 -- that fades down after it's
 -- triggered
 
 hit[2]=(note[2])
 hit[4]=(volu[2])
 
 if hit[2]>2 then
  hitf[3]=2 --hit[2]
 else
  hitf[3]=hitf[3]*0.2
 end
 if hit[4]>2 then
  hitf[4]=0.4
 else
  hitf[4]=hitf[4]*0.2
 end
 
 -- deprecated, but another way
 -- to keep track of things
 -- didn't work well, though
 if note[2]==36 and ti[1]+dt*2>tt then 
  rnd_efu=rnd_efu+1 
  ti[1]=tt
 end
 if rnd_efu>8 then rnd_efu=0 end

 t=t+0.0005
 if rec==1 then
  update_music_stats()
 end
 yield() --coroutine stuff \😐/
end

f=0
cur_ef=nil
function _draw()
--[

 -- keep a copy of the screen
 -- outside of screen memory
 -- so we can do stuff like
 -- fades and things just by
 -- poking the memory around
 memcpy(0x1000,0x6000,0x2000)
 f=f+1
 --holdframe()
 --if f%4==0 then
 --poke(0x5f5e,0b10101010)
 if efu[stat(24)+1]==1 then 
  cur_ef=cocreate(efu_xor) 
 end
 if efu[stat(24)+1]==0 then
  cur_ef=cocreate(efu_lines) 
 end
 if efu[stat(24)+1]==2 then
  cur_ef=cocreate(efu_lines) 
 end
 if efu[stat(24)+1]==3 then
  if stat(21)<=8 then
   cur_ef=cocreate(efu_border)
  else
  	cur_ef=cocreate(efu_lines)
  end 
 end
 if efu[stat(24)+1]==4 then
  if stat(21)<=8 then
   cur_ef=cocreate(efu_border)
  else
  	cur_ef=cocreate(efu_xor)
  end 
 end
	
 if cur_ef and costatus(cur_ef)!= 'dead' then
  if efu[stat(24)+1]==1 then
   if f%2==0 then coresume(cur_ef) end
  else
   coresume(cur_ef)
  end
 else
  cur_ef=nil
 end 
 if rec==1 then
 rectfill(0,0,31,128,0)
  draw_music_stats()
  print(rnd_efu,0,6*6,8)
  print(hit[2])
  print(note[2])
  print(inst[2])
  print(stat(7))
 end
 rec=btn()>>>12&1
 --]]
 --a pixel in the top left
 --for keeping an eye on fps
 --(blinks between two colors) 
 poke(0x6000,flr(f)*128)
end

function debug_palette()
 rectfill(0,0,128,128,0)
 for i=0,15 do
  print(@(0x5f10+i),0,6*i,i)
  
  print(i,4*4,6*i,i)
 end
end 

function efu_smear()
 local i
 for i=1,127 do
  smcpy(0x6000+i*63-rnd(2+(stat(24)%6)*0.25),0x1000+i*63-rnd(2+(stat(24)%8)*0.25),rnd(128)) 
 end 
end

function efu_border()
 rectfill(1,0,3,128,8-rnd(4))
 line(126-rnd(120),0,128-rnd(120),128,8+rnd(4))
 efu_wiggle(10)
 efu_smear()
 yield()
end


-- polyline scene
function efu_lines()
 local i
 local x
 local dx={}
 local dy={}
 local fv=40
 local p
 local sc
 cls(bg)
 if efu[stat(24)+1]!=2 then 
  efu_smear()
 end
 efu_wiggle(1.5)
 for i=1,#px do
  ii=i+1
  if ii>#px then ii=1 end
  iii=ii+1
  if iii>#px then iii=1 end
  dx[1]=(px[i]*(fv/pz[i])+64)
  dy[1]=(py[i]*(fv/pz[i])+64)
  dx[2]=(px[ii]*(fv/pz[ii])+64)
  dy[2]=(py[ii]*(fv/pz[ii])+64)
  dx[3]=(px[iii]*(fv/pz[iii])+64)
  dy[3]=(py[iii]*(fv/pz[iii])+64)
  line(dx[1],dy[1],dx[2],dy[2],8+rnd(2)) 
  line(dx[2],dy[2],dx[3],dy[3],8-rnd(2))
 end
 if hit[2]>35 then
  sc=640
  rand_poly(-100,-100,50,100,100,250)
 end
 yield()
end

-- xor twisting and stuff
-- needs optimisation
function efu_xor()
	local x
	local y
	local xx
	local yy
	local mem
	local c
	local p
	local p1
	local p2
	local b=efv[2]
	cls(bg)
	for mem=0,0x1fff do
	 xx=mem%64-32
		if flr(xx)==0 then
 	 dd=rnd(dis)
		end
 	if flr(mem>>>6)%2==0 then
	  yy=flr(mem>>>6)-64
 	 x=xoff+(xx*cot-(yy/2)*sit)+(xx*yy)*0.01
   y=yoff+(xx*sit+(yy/2)*cot)+(yy*xx)*0.01
 		c=(x^^y)*0.4+t*300--(1+sin(t)*8)
 		p=@(mem-0x5000-hit[1])
 		p1=(hit[1]*4*(p+1))|((c&b))
 		p2=(hit[1]*4*(p+1))|((c&b))
  	spoke(mem+dis+0x6000,((p2<<4)|p1)) 
  	spoke(mem+64-dis+0x6000,((p2<<4)|p1))
   
   --[[ one way to optimize
   --   is to just cut down the
   --   resolution, these would
   --   fill the black space
   --
   spoke(1+mem+dis+0x6000,((p2<<4)|p1)) 
  	spoke(1+mem+64-dis+0x6000,((p2<<4)|p1))
  	spoke(mem+128+dis+0x6000,((p2<<4)|p1)) 
   spoke(mem+192-dis+0x6000,((p2<<4)|p1))
  	spoke(1+mem+128+dis+0x6000,((p2<<4)|p1)) 
  	spoke(1+mem+192-dis+0x6000,((p2<<4)|p1))
 	 --]]
 	end
	end
	yield()
end


--horisontal screen wiggling
function efu_wiggle(str)
 for i=1,127 do
  smcpy(0x6000+i*63+sin(tt+i*(0.02*hit[4]))*(note[3]/18.)*str,0x6000+i*63,64) 
 end
end

--wrapping screen memory poke
function spoke(mem,val)
 while mem<0x6000 do
  mem=mem+0x1fff 
 end
 while mem>0x7fff do
  mem=mem-0x1fff
 end
 poke(mem,val)
end

--memcopy that doesn't overflow
function smcpy(to_mem,fr_mem,len)
 if to_mem<0x6000 then to_mem=0x6000 end
 if to_mem>0x7ffe then
  to_mem=0x7ffe
  len=1
 end
 if to_mem+len>0x7fff then len=0x7fff-to_mem end
 --if fr_mem<0x6000 then fr_mem=0x6000 end
 if fr_mem>0x7ffe then
  fr_mem=0x7ffe
  len=1
 end
 if fr_mem+len>0x7fff then len=0x7fff-fr_mem end
 
 memcpy(to_mem,fr_mem,len)

end

-->8
--some rudimentary 3d-shapes

polys=5
px={}
py={}
pz={}
vx={}
vy={}
vz={}
prot={x=0.0,y=0.0,z=0.0}

function rand_poly(x1,y1,z1,x2,y2,z2)
 local i
 
 for i=1,#px do
  del(px,px[i])
  del(py,py[i])
  del(pz,pz[i])
  del(vx,vx[i])
  del(vy,vy[i])
  del(vz,vz[i])
 end
 for i=1,polys do
  add(px,mid(x1,x1+rnd(x2-x1),x2))
  add(py,mid(y1,y1+rnd(y2-y1),y2))
  add(pz,mid(z1,z1+rnd(z2-z1),z2))
  add(vx,0)
  add(vy,0)
  add(vz,0)
 end
end

function update_poly(hit)
 for i=1,#px do
  px[i]=px[i]
  py[i]=py[i]+vy[i]
  pz[i]=pz[i]
  vy[i]=vy[i]+1
  if vy[i]>10 then vy[i]=10 end
  if py[i]+vy[i]>100 then vy[i]=-vy[i]*0.9 end
 end
 prot.x=prot.x*0.6
 prot.y=prot.y*0.6
 prot.z=prot.z*0.6
end

function stop_poly()
 for i=1,#px do
  vx[i]=0
  vy[i]=0
  vz[i]=0
 end
 prot.x=0
 prot.y=0
 prot.z=0
end

function rotate_poly(x,y,z)
 local cox=cos(x)
 local six=sin(x)
 local coy=cos(y)
 local siy=sin(y)
 local coz=cos(z)
 local siz=sin(z)
 local xx={}
 local yy={}
 local zz={}
 xx[4]=0
 yy[4]=0
 zz[4]=150
 cox=cos(x)
 six=sin(x)
 coy=cos(y)
 siy=sin(y)
 coz=cos(z)
 siz=sin(z)
 
 for i=1,#px do
  zz[5]=pz[i]-zz[4]
  xx[1]= (xx[4])+px[i]*coy-zz[5]*siy
  yy[1]= (yy[4])+py[i]
  zz[1]=-(zz[4])+px[i]*siy+zz[5]*coy
  
  xx[2]=xx[1]
  yy[2]=yy[1]*cox-zz[1]*six
  zz[2]=yy[1]*six+zz[1]*cox
  
  px[i]=xx[2]*coz-yy[2]*siz
  py[i]=xx[2]*siz+yy[2]*coz
  pz[i]=zz[2]+zz[4]*2
 end
end
-->8
--music helpers


function stats(i,k,x,y,text)
 local note={}
	print("",x,y,7)
	print(text)
	for li=i,k do
	 color(8)
	 print(stat(li))
	 note=get_note(li-16)
	 rectfill(x+8,y+6*(li-15),x+8+note[3]*2+1,y+6*(li-15)+4,note[3])
	end
end

function update_music_stats()

	ch1=get_note(0)
	ch2=get_note(1)
	ch3=get_note(2)
	ch4=get_note(3)
end

--useful for debugging music
--things but definitely not
--needed in the production
function draw_music_stats()
 print(stat(24),4*5,0,8)
	stats(16,19,0,0,"ptrn")
	--stats(20,23,4*5,0,"note")
	--stats(24,24,0,6*5,"current")
	--stats(25,25,4*8,6*5,"played")
--	stats(26,26,0,6*7,"ticks")
--[[	
	for i=1,4 do
		rates[i]=(@0x5f40&(0b0001<<(i-1))!=0)
		reverb[i]=(@0x5f41&(0b0001<<(i-1))!=0)
		distorsion[i]=(@0x5f42&(0b0001<<(i-1))!=0)
		filter[i]=(@0x5f43&(0b0001<<(i-1))!=0)
	end
	print("half speed",4*15,0,7)
	print("",4*15,6,8)
	foreach(rates,print)
	color(7)
	print("reverb")
	color(8)
	foreach(reverb,print)
	color(7)
 print("distorsion")
	color(8)
	foreach(distorsion,print)
	color(7)
	print("lowpass filter")
	color(8)
	foreach(filter,print)
--]]
	print("█",0,122-ch1[1]/2,ch1[3])
	rectfill(1,122-ch1[1]/2,5,128-ch1[1]/2,0)
	print(ch1[4],0+2,122-ch1[1]/2,(ch1[2]+8)*(ch1[3]&1))
	print("█",2*3+1,122-ch2[1]/2,ch2[3])
	rectfill(8,122-ch2[1]/2,12,128-ch2[1]/2,0)
	print(ch2[4],2*3+3,122-ch2[1]/2,(ch2[2]+8)*(ch2[3]&1))
	print("█",2*6+2,122-ch3[1]/2,ch3[3])
	rectfill(15,122-ch3[1]/2,19,128-ch3[1]/2,0)
	print(ch3[4],2*6+4,122-ch3[1]/2,(ch3[2]+8)*(ch3[3]&1))
	print("█",2*9+3,122-ch4[1]/2,ch4[3])
	rectfill(22,122-ch4[1]/2,26,128-ch4[1]/2,0)
	print(ch4[4],2*9+5,122-ch4[1]/2,(ch4[2]+8)*(ch4[3]&1))
end


-- returns a table of four 
-- pitch, instrument, vol, fx
function get_note(ch)
 local sf = stat(16+ch)
 local tm = stat(20+ch)
 local addr = %(0x3200 + 68*sf + 2*tm)
 --      bitmap    sfx
 --       for         vol
 --      notes           inr
 --                         pitch   
 local pitch = (0b0000000000111111&addr)
 local instr = (0b0000000111000000&addr)>>>6
 local vol   = (0b0000111000000000&addr)>>>9
 local fx    = (0b0111000000000000&addr)>>>12
 return { pitch, instr, vol, fx }
end
__gfx__
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00700700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00077000015d67a80000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00077000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00700700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__label__
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000009900000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000n0000000997900000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000n000n0n0770777777777777766000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000009770000000n770000000n770n7700000n77666n66000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000009770n770n777760000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000099777777nn77nn770000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000907700n09777009000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000n00770n097n0977690977600900000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000n00000nnnn99nnnn7777n900nn9977699966777977000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000n0n000979077n000779000n6n0n796779097907796900000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000nn0007770099000666nn9907777700000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000099909977907700000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000099900770000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000n0900077n0900077900077000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000n00900n00770900770n90770077779n0990660000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000n000990099n0770977007777000966000066000000000000000000000000000000
000000000000000000000000000000000000000000000000000099nn00nnnn900090977700007700000000000000000000000000000000000000000000000000
000000000n999000000nnn0000000000000000000000000000000n00000n00990000990077000077000000000000000000000000000000000000000000000000
0000nnnnnnnnn0000000000000000000000000000090000000n009n000n00999n90009090909000n000n0n000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000090n000n0000009n99009000909090n000000090n0000000000000000000000000000000000000
000n000n000nn9n9999n0000000000000000000000000000000000n0n00000000000090900000090n00000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000n000000000000000900000000n00000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000n0n000009000009000909000909n99n0900900900990009990nn00090n090n09000n090000
0000000000000000000000000000000000000000000000000000nn000000nn0000990000990000000n0000090900090909000000000000000000000000000000
000000000000000000000000000000000000000000000000000n00000000000000000000090000000n0000000000000000000000000000000000000000000000
000000000000000000090000000000000000000000000000n0n090n000009000n00000909090909090n090909000n090n0000000000000000000000000000000
0000000000000000000000000000000000000009090n00000n000009000n0000090909009099900000n000000000000000000000000000000000000000000000
00000000000000000000000000000000000000n0n0n000909000900000000000090900000009000000n000000000000000000000000000000000000000000000
0000000000000000000000900090000000n0n0000000n00090000000000000909000090990009009000n00000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000900000000000000000000000000n900000n00000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000900000000000000000000000000n900000n00000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000009000n000000000000000000000n0909090n000900000909090909090909090n000000000009000
000000000000000000000000000000000000000000000000009000n000n00000n090009000nn00090009n0099n099909000009000000999009n0009000900000
00000000000000000000000000000000000000000000000000900000n0000000n00000n0900n900999n0n900nn90090909090n09000000009n90009000000000
000090909099909000000000000000000000000000000000000900000n00000n0n0900090n0n000099000n90000000000n000n000n0000000009000000000979
0n790nnn0099990n007070000000000000000000000000000n0900000000000000000000090n000909090n0n0n0n000900000000090000000000000000000000
0000000000000000000000000000000000000000n00000n0000090000000000n00000900000n090909090n0n090n090909000000000000000000000000000000
000000000000000000000000000000000000000000000000000090000000000000000000009n900n909000n00000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000009000000000000000000000n99909909909n000900900n9900000000000000000000000000000
00000000000000000000000000000000000000000000000n0n0009000000000000n0009090n09090909090n090900090n0900000000000000000000000000000
00000000000000000000000000000000000000000000000n0000090n000000000000n00000n090909099990n9099nn9909000000000000000000000000000000
00000000000000000000000000000000000000000000000000n0n09000900000000000n090n090009000090n0909n9099009n9n999n9nn0n09090n0900000000
0000000000000000000000000000000000000000000000000000009000n000n00000n00090n000900000909nn0n099009999n0909n999nn9n000990099990000
000000000000000000000000000000000000000000000000000000900000n00000n0000000n0000000000090n000909090900090000000000000000000000000
0000000000000000000000000000000000000000000000000000000900000n000n0000000n000n0000000n09n000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000900000000000n00090n09090099009999n0n0909099000090009000009000000000000000
00000000000000000000000000000000000000000000000000000000900n0n00000900009n009900009900009n00000000000000000000000000000000000000
0000000000000000000000000000000000000000n000n00000n0000n9n090n0n090909000n000000090000000n00000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000900n9n00900nn9n0090n0n000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000n00000900090n000909000n000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000n0900000n000n000nn09900n990n0009000n0n00090000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000900000n000n000n000n09000n0909000n0n0909000n0n000n000000000000000000000
000000000000000000000000000000000000000000000000000000000090000000000000n000n0900000n00000n090000090n0n0n0n09090n00090n0n0n09000
00n00090n000000000000000000000000000000000000000000000000090000000000000n0000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000n090n000n000990n0n900n009900000nn0000n0000009n0000000000000000000000000
00000000000000000000000000000000000000000000000000000n0n0n09000900n0n090n000n000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000n0n0n99nn90n0n99090900000n00000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000n09090n0000000n00n000090090000000n00000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000n000n000900n0009000n00000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000090009000n0n090n00090n00090n0n0n000n09000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000009090n090n0909090n0n0n090n0n0009000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000090000000nnn000n0090n009n9090n0n90n0n0n0900000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000090000000n000n0009000n000909090n0n0n090n0n0000090n00000000000000000
000000000000000000000000000000000000000000000000000000000000009000n000n0009000n090n0n00090n0900000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000009000n00n00000n000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000n0009009000n000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000n0n000n0n000909000n000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000n000n0n0n0n0909000n0n000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000n0n090000n009000090090000nn0n0nnnn0990000n0000000000000000000000000000000000000000000000000000000000
0000000000000000000000000n0000000000000000000000000000000000000009000n0000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000009000n0n00090n000n0000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000900n0000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000900n0000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000900n0000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000n99nnn009900009n9n00n900nn009n0000990000009n0000n90000000000000
00000000000000000000000000000000000000000000000000000000000nnn9nn9n9n00000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000090000000n00000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000900000n0n09090000000n00000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000

__map__
0000000202000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000020000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__sfx__
0001000418070130701b0701f07000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0101000418070140701b0702007000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0101000418070140701b0702407000002000020000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010300202455311010110201103011040110501105011050306200c6351101011010110201103011040110501105011050110501105024553117101172011730306200c635110101101011020110301104011050
01030020245530d0100d0200d0300d0400d0500d0500d050306200c6350d0100d0100d0200d0300d0400d0500d0500d0500d0500d050245530d7100d7200d730306200c6350d0100d0100d0200d0300d0400d050
01030020245530f0100f0200f0300f0400f0500f0500f050306200c6350f0100f0100f0200f0300f0400f0500f0500f0500f0500f050245530f7100f7200f730306200c6350f0100f0100f0200f0300f0400f050
01030020245530507511000110001100011000110001100030620110501105011050110501105011050110501105011050110501105024553117101172011730306200c635110101101011020110301104011050
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01030020245530d3100d3200d3300d3400d3500d3500d350306200c6350d3100d3100d3200d3300d3400d3500d3500d3500d3500d350245530d3100d3200d330306200c6350d3100d3100d3200d3300d3400d350
01030020245530f3100f3200f3300f3400f3500f3500f350306200c6350f3100f3100f3200f3300f3400f3500f3500f3500f3500f350245530f3100f3200f330306200c6350f3100f3100f3200f3300f3400f350
010300202455311310113201133011340113501135011350306200c6351131011310113201133011340113501135011350113501135024553113101132011330306200c635113101131011320113301134011350
010300202455319010190201903019040190501905019050306200c6351901019010190201903019040190501905019050190501905024553197101972019730306200c635190101901019020190301904019050
01030020245531b0101b0201b0301b0401b0501b0501b050306200c6351b0101b0101b0201b0301b0401b0501b0501b0501b0501b050245531b7100370030600306200c6351b0001b0001d050160511305111051
01030020245530507511000110001100011000110001100030620113501135011350113501135011350113501135011350113501135024553113101132011330306200c635113101131011320113301134011350
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010300202455319310193201933019340193501935019350306200c6351931019310193201933019340193501935019350193501935024553193101932019330306200c635193101931019320193301934019350
01030020245531b3101b3201b3301b3401b3501b3501b350306200c6351b3101b3101b3201b3301b3401b3501b3501b3501b3501b350245531b7100370030600306200c6351b0001b0001d050160511305111051
01030020240630572505000050001c61505000050000000024620186300c645006251c615000001c615000001c615000001c6151800024063057250570005700246201863000645006251c615000001c61500000
01030020240630572505000050001c61505000050000500024620186350c64500625240630572504600180000460000000046001800024620186350c64500625306000c6001c61518000306200c635306200c635
011000002405024050240502405024050240502405024050240502405024050240502405024050240502405024050240502405024050240502405024050240502405024050240502405024050240502405024050
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010300200000000000000000000000000000000000000000298302982029820298202982029820298202982000800000000000000000298302982029820298202982029820298202982000000000000000000000
010300202983029820298202982029820298202982029820000000000000000000002983029820298202982029820298202982029820000000000000000000002983029820298202982029820298202982029000
0103002000700007000070000000000000000000000000003075030755007002470033750337553071030715357503575533710337152e7502e755357103571530750307552e7102e71533750337553071030715
01030020377503775533710337152e7502e755377103771530750307552e7102e71537750377553071030715387503875537710377152c7502c755387103871530750307552c7102c71533750337553071030715
010300202070000700337103371531750317550870000700337503375531710317153775037755337103371538750387553771037715317503175538710387153375033755317103171538750387553371033715
010300203a7503a75538710387152e7502e7553a7103a71531750317552e7102e71537750377553171031715387503875537710377152c7502c755387103871531750317552c7102c71533750337553171031715
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0103002000000000000000000000298302982029820298202982029820298202982000000000000000000000298302982029820298202980029800298002b000298302982029820298202980029800298002b000
010300202b9302b9202b9202b9202b9202b9202b9202b920000000000000000000002b9302b9202b9202b9202b9202b9202b9202b920000000000000000000002b9302b9202b9202b9202b9202b9202b92029920
010300202993029920299202992029920299202992029920000000000000000000002993029920299202992029920299202992029920000000000000000000002993029920299202992029920299202992029000
0103002029a3029a2029a2029a2029a2029a2029a2029a200000000000000000000029a3029a2029a2029a2029a2029a2029a2029a200000000000000000000029a3029a2029a2029a2029a2029a2029a2029000
__music__
01 601a2048
00 601a2148
00 601a2a48
00 601b2948
00 601a2148
00 601a2848
00 601a2b48
00 601b2948
00 0d1a2022
00 0a1a2123
00 0b1a2a24
00 0c1b2925
00 0a1a2122
00 0a1a2823
00 131a2b24
00 141b2925
00 0d1a0d22
00 0a1a0a23
00 0b1a0b24
00 0c1b0c25
00 0a1a0a22
00 0a1a0a23
00 131a1324
00 141b1425
01 151a0d22
00 121a0a23
00 101a0b24
00 111b0c25
00 121a0a22
00 121a0a23
00 181a1324
00 191b1425
00 0d1a2022
00 0a1a2123
00 0b1a2a24
00 0c1b2925
00 0a1a2122
00 0a1a2823
00 131a2b24
02 141b2925

