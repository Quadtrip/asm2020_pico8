pico-8 cartridge // http://www.pico-8.com
version 27
__lua__
-- main


-- which effect is played in ptr

-- efu 7 is the raycast, so
-- uncomment the top row
-- for easy testing of it
efu={--7,7,7,7,7,7,7,7,
     3,4,3,4,3,4,3,4,
     3,4,4,4,4,4,4,4,
     5,6,6,6,6,6,6,6,
     1,2,2,2,2,2,2,2,
     3,4,4,4,3,4,4,4,
     0,0,0,0,0,0,0,0}
     
-- select by using efu[stat(24)]
-- and show scene based on that     
     
-- control values for effects
efv={0b00000000, -- efu_lines
     0b00001011, -- efu_xor
     0b11111111} -- efu 2? more?
 

oldt = 0
t = 0

rec = -1

dpre = {}

sp = {}
zbuf = {}
spord = {}
spdist = {}

lxi = {}
rxi = {}
lyi = {}
ryi = {}

px = 10
py = 10

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


    
bg=0

tt=.0
dt=.0
ot=.0
rec=-1

local bun2 = {
	x = 38+4,
	y = 2.5,
	s_x = 80,
	s_y = 16,
	s_w = 16,
	s_h = 16,
	pi = 1,
	goal = {x=12,y=3}
}

ss = {
	x = 0,
	y = 0
}

se = {
	x = px,
	y = py
}



function _init()

 -- raycaster

	for i = 0,128 do
		zbuf[i] = 0
	end

 -- sprites

	add(sp,bun)
	add(sp,bun2)
	
	for y = 1, h+64, 1 do
		cd = h / (2.0 * (y-1) - h)
		dpre[y] = cd
	end

	-- voxel
	
	for i = 1, 256 do
	 lxi[i] = 0
	 rxi[i] = 0
	 lyi[i] = 0
	 ryi[i] = 0
	end

	-- effuu
	
	-- create a polyline
 rand_poly(-100,-100,50,100,100,250)
 --
 --[[
 -- palette gradient
 poke4(0x5f10,0x8582.8000)
 poke4(0x5f14,0x0706.8605)
 poke4(0x5f18,0x088e.0987)
 poke4(0x5f1c,0x8084.0288)
 poke(0x5f2e,1)
 --]]
 redpal()
 
	-- music init
 poke(0x5f40,0b1111) --speed
	poke(0x5f41,0b0000) --reverb
	poke(0x5f42,0b0000) --dist
	poke(0x5f43,0b0001) --lowpass

	music(0)
	

end


function keylogic()
	moves = ft * 4.0
	rots = ft * 0.25

 -- keys
 if (btn(4)) then 
 	rec = -1
 end
 
 if (btn(5)) then
 	rec = 1
 end
 
	if (btn(2)) then -- up
		moved = 1
		t1 = mget(flr(px+dirx * moves),flr(py))
  t2 = mget(flr(px),flr(py + diry * moves))
		if (t1 == 0 or t1 == 20) then px += dirx * moves end
		if (t2 == 0 or t2 == 20) then py += diry * moves end
	end

	if (btn(3)) then -- down
		moved = 1
		t1 = mget(flr(px-dirx * moves),flr(py))
		t2 = mget(flr(px),flr(py - diry * moves))
		if (t1 == 0 or t1 == 20) then px -= dirx * moves end
		if (t2 == 0 or t2 == 20) then py -= diry * moves end
	end
	
	if (btn(0)) then -- left
		oldx = dirx
		dirx = dirx * cos(-rots) - diry * sin(-rots)
		diry = oldx * sin(-rots) + diry * cos(-rots)
		oldpx = planex
		planex = planex * cos(-rots) - planey * sin(-rots)
		planey = oldpx * sin(-rots) + planey * cos(-rots)
		a = a - ft*0.1
	end

	if (btn(1)) then -- right
		oldx = dirx
		dirx = dirx * cos(rots) - diry * sin(rots)
		diry = oldx * sin(rots) + diry * cos(rots)
		oldpx = planex
		planex = planex * cos(rots) - planey * sin(rots)
		planey = oldpx * sin(rots) + planey * cos(rots)
		a = a + ft*0.1
	end
end

function spritelogic()
	 for s in all(sp) do

			if moved == 1 then
				ss.x = flr(s.x)
				ss.y = flr(s.y)
				se.x = flr(px)
				se.y = flr(py)
	
				path = find_path(ss,se,
																					manhattan_distance,
																					flag_cost,
																					map_neighbors,
																					function (node) return shl(node.y,8)+node.x end,
																					nil)
		end
	
		ssp = 1.0
		thr = 0.01
	
		if path then
			if (abs(s.x-se.x) > thr and abs(s.y-se.y) > thr and s.pi == 0) then s.pi=1 end

			if (s.pi > 0) then
				s.path = path
			
				pp = s.path[s.pi]
	
				if (s.x < pp.x) then s.x += ft*ssp	end
				if (s.y < pp.y) then s.y += ft*ssp	end
				if (s.x > pp.x) then s.x -= ft*ssp	end
				if (s.y > pp.y) then s.y -= ft*ssp	end
				if (abs(s.x-pp.x) < thr and abs(s.y-pp.y) < thr) then s.pi+= 1 end
		
				if(s.pi >= (#s.path)-1) then s.pi = 0 end
			end
		end

	end

	moved = 0

end

calcvoxel = 1

function do_update()


	oldt = t
	t=time()
	ft = (t-oldt)
	
	keylogic()
	spritelogic()

end


frame = 0
effu = 0
snc=nil


function _update60()
	do_update()

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
 --[[
function _draw2()
	cls(0)

	frame = frame + 1
	
	if (stat(25)%4==0) and (stat(25)%8==0) and effu==1 then
	 effu -= 1
	end
	if (stat(25)%4==0) and (stat(25)%8==4) and effu!=1 then
		effu += 1

		px = 56
		py = 10.5

	end

	if effu == 0 then
		voxel()
	end

	if effu == 1 then

	palt(0,false)

	sky()
	raycast()
	casterpal()
	
	end

	if rec == 1 then
 end
	
end
--]]

f=0
cur_ef=nil
function _draw()
--[
 --memcpy(0x1000,0x6000,0x2000)
 f=f+1
 if (efu[pattern]==0) cur_ef=nil 
 if (efu[pattern]==1) cur_ef=cocreate(efu_xor) 
 if (efu[pattern]==2) cur_ef=cocreate(efu_xor)
 if (efu[pattern]==3) cur_ef=cocreate(efu_lines) 
 if (efu[pattern]==4) cur_ef=cocreate(efu_lines)  
 if (efu[pattern]==5) cur_ef=cocreate(voxel)  
 if (efu[pattern]==6) cur_ef=cocreate(voxel)
 if (efu[pattern]==7) then
  cur_ef=cocreate(raycast)
  sky()
 end
 
  if stat(21)<=8 then
   if (efu[pattern]>0 and efu[pattern]%2==1) then
    cur_ef=cocreate(efu_border)
   end 
  end
	
 if cur_ef and costatus(cur_ef)!= 'dead' then
  if efu[pattern]==1 then
   if (f%2==0) coresume(cur_ef)
  else
   coresume(cur_ef)
  end
 else
  cur_ef=nil
 end 
 
 --debug info
 if rec==1 then
  rectfill(0,0,31,128,0)
--  draw_music_stats()
  print(hit[2],0,6*6,8)
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

--[[
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
--]]

-->8
--some rudimentary 3d-shapes

polys=5
pox={}
poy={}
poz={}
vx={}
vy={}
vz={}
prot={x=0.0,y=0.0,z=0.0}

function rand_poly(x1,y1,z1,x2,y2,z2)
 local i
 
 for i=1,#pox do
  del(pox,pox[i])
  del(poy,poy[i])
  del(poz,poz[i])
  del(vx,vx[i])
  del(vy,vy[i])
  del(vz,vz[i])
 end
 for i=1,polys do
  add(pox,mid(x1,x1+rnd(x2-x1),x2))
  add(poy,mid(y1,y1+rnd(y2-y1),y2))
  add(poz,mid(z1,z1+rnd(z2-z1),z2))
  add(vx,0)
  add(vy,0)
  add(vz,0)
 end
end

function update_poly(hit)
 for i=1,#pox do
  pox[i]=pox[i]
  poy[i]=poy[i]+vy[i]
  poz[i]=poz[i]
  vy[i]=vy[i]+1
  if vy[i]>10 then vy[i]=10 end
  if poy[i]+vy[i]>100 then vy[i]=-vy[i]*0.9 end
 end
 prot.x=prot.x*0.6
 prot.y=prot.y*0.6
 prot.z=prot.z*0.6
end

function stop_poly()
 for i=1,#pox do
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
 
 for i=1,#pox do
  zz[5]=poz[i]-zz[4]
  xx[1]= (xx[4])+pox[i]*coy-zz[5]*siy
  yy[1]= (yy[4])+poy[i]
  zz[1]=-(zz[4])+pox[i]*siy+zz[5]*coy
  
  xx[2]=xx[1]
  yy[2]=yy[1]*cox-zz[1]*six
  zz[2]=yy[1]*six+zz[1]*cox
  
  pox[i]=xx[2]*coz-yy[2]*siz
  poy[i]=xx[2]*siz+yy[2]*coz
  poz[i]=zz[2]+zz[4]*2
 end
end



-->8
-- helper functions

-- pathfinder
-- by @casualeffects

-- i minimized the number of
-- tokens as far as possible
-- without hurting readability
-- or performance. you can save
-- another four tokens and a
-- lot of characters by
-- minifying if you don't care
-- about reading the code.

-- returns the shortest path, in
-- reverse order, or nil if the
-- goal is unreachable.
--
-- from the graphics codex
-- http://graphicscodex.com
function find_path
(start,
 goal,
 estimate,
 edge_cost,
 neighbors, 
 node_to_id, 
 graph)
 
 -- the final step in the
 -- current shortest path
 local shortest, 
 -- maps each node to the step
 -- on the best known path to
 -- that node
 best_table = {
  last = start,
  cost_from_start = 0,
  cost_to_goal = estimate(start, goal, graph)
 }, {}

 best_table[node_to_id(start, graph)] = shortest

 -- array of frontier paths each
 -- represented by their last
 -- step, used as a priority
 -- queue. elements past
 -- frontier_len are ignored
 local frontier, frontier_len, goal_id, max_number = {shortest}, 1, node_to_id(goal, graph), 32767.99

 -- while there are frontier paths
 while frontier_len > 0 do

  -- find and extract the shortest path
  local cost, index_of_min = max_number
  for i = 1, frontier_len do
   local temp = frontier[i].cost_from_start + frontier[i].cost_to_goal
   if (temp <= cost) index_of_min,cost = i,temp
  end
 
  -- efficiently remove the path 
  -- with min_index from the
  -- frontier path set
  shortest = frontier[index_of_min]
  frontier[index_of_min], shortest.dead = frontier[frontier_len], true
  frontier_len -= 1

  -- last node on the currently
  -- shortest path
  local p = shortest.last
  
  if node_to_id(p, graph) == goal_id then
   -- we're done.  generate the
   -- path to the goal by
   -- retracing steps. reuse
   -- 'p' as the path
   p = {goal}

   while shortest.prev do
    shortest = best_table[node_to_id(shortest.prev, graph)]
    add(p, shortest.last)
   end

   -- we've found the shortest path
   return p
  end -- if

  -- consider each neighbor n of
  -- p which is still in the
  -- frontier queue
  for n in all(neighbors(p, graph)) do
   -- find the current-best
   -- known way to n (or
   -- create it, if there isn't
   -- one)
   local id = node_to_id(n, graph)
   local old_best, new_cost_from_start =
    best_table[id],
    shortest.cost_from_start + edge_cost(p, n, graph)
   
   if not old_best then
    -- create an expensive
    -- dummy path step whose
    -- cost_from_start will
    -- immediately be
    -- overwritten
    old_best = {
     last = n,
     cost_from_start = max_number,
     cost_to_goal = estimate(n, goal, graph)
    }

    -- insert into queue
    frontier_len += 1
    frontier[frontier_len], best_table[id] = old_best, old_best
   end -- if old_best was nil

   -- have we discovered a new
   -- best way to n?
   if not old_best.dead and old_best.cost_from_start > new_cost_from_start then
    -- update the step at this
    -- node
    old_best.cost_from_start, old_best.prev = new_cost_from_start, p
   end -- if
  end -- for each neighbor
  
 end -- while frontier not empty

 -- unreachable, so implicitly
 -- return nil
end

function ce_heap_sort(data,xx,yy)
 if (#data==0) return
 local n = #data
 local p_pos={x=xx,y=yy}
 
 for d in all(data) do
  local dx=(d.x-p_pos.x)/10
  local dy=(d.y-p_pos.y)/10
  d.key=dx*dx+dy*dy
 end

 -- form a max heap
 for i = flr(n / 2) + 1, 1, -1 do
  -- m is the index of the max child
  local parent, value, m = i, data[i], i + i
  local key = value.key 
  
  while m <= n do
   -- find the max child
   if ((m < n) and (data[m + 1].key < data[m].key)) m += 1
   local mval = data[m]
 
   if (key < mval.key) break
   data[parent] = mval
   parent = m
   m += m
  end
  data[parent] = value
 end 

 -- read out the values,
 -- restoring the heap property
 -- after each step
 for i = n, 2, -1 do
  -- swap root with last
  local value = data[i]
  data[i], data[1] = data[1], value

  -- restore the heap
  local parent, terminate, m = 1, i - 1, 2
  local key = value.key 
  
  while m <= terminate do
   local mval = data[m]
   local mkey = mval.key
   if (m < terminate) and (data[m + 1].key < mkey) then
    m += 1
    mval = data[m]
    mkey = mval.key
   end
   if (key < mkey) break
   data[parent] = mval
   parent = m
   m += m
  end  
  
  data[parent] = value
 end
end

-- makes the cost of entering a
-- node 4 if flag 1 is set on
-- that map square and zero
-- otherwise
function flag_cost(from, node, graph)
 return fget(mget(node.x, node.y), 1) and 4 or 1
end


-- returns any neighbor map
-- position at which flag zero
-- is unset
function map_neighbors(node, graph)
 local neighbors = {}
 if (not fget(mget(node.x, node.y - 1), 0)) add(neighbors, {x=node.x, y=node.y - 1})
 if (not fget(mget(node.x, node.y + 1), 0)) add(neighbors, {x=node.x, y=node.y + 1})
 if (not fget(mget(node.x - 1, node.y), 0)) add(neighbors, {x=node.x - 1, y=node.y})
 if (not fget(mget(node.x + 1, node.y), 0)) add(neighbors, {x=node.x + 1, y=node.y})
 return neighbors
end

-- estimates the cost from a to
-- b by assuming that the graph
-- is a regular grid and all
-- steps cost 1.
function manhattan_distance(a, b)
 return abs(a.x - b.x) + abs(a.y - b.y)
end

 local f={}
 local p={}
 local permutation={151,160,137,91,90,15,
  131,13,201,95,96,53,194,233,7,225,140,36,103,30,69,142,8,99,37,240,21,10,23,
  190,6,148,247,120,234,75,0,26,197,62,94,252,219,203,117,35,11,32,57,177,33,
  88,237,149,56,87,174,20,125,136,171,168,68,175,74,165,71,134,139,48,27,166,
  77,146,158,231,83,111,229,122,60,211,133,230,220,105,92,41,55,46,245,40,244,
  102,143,54,65,25,63,161,1,216,80,73,209,76,132,187,208,89,18,169,200,196,
  135,130,116,188,159,86,164,100,109,198,173,186,3,64,52,217,226,250,124,123,
  5,202,38,147,118,126,255,82,85,212,207,206,59,227,47,16,58,17,182,189,28,42,
  223,183,170,213,119,248,152,2,44,154,163,70,221,153,101,155,167,43,172,9,
  129,22,39,253,19,98,108,110,79,113,224,232,178,185,112,104,218,246,97,228,
  251,34,242,193,238,210,144,12,191,179,162,241,81,51,145,235,249,14,239,107,
  49,192,214,31,181,199,106,157,184,84,204,176,115,121,50,45,127,4,150,254,
  138,236,205,93,222,114,67,29,24,72,243,141,128,195,78,66,215,61,156,180
 }

 for i=0,255 do
  local t=shr(i,8)
  f[t]=t*t*t*(t*(t*6-15)+10)

  p[i]=permutation[i+1]
  p[256+i]=permutation[i+1]
 end

 local function lerp(t,a,b)
  return a+t*(b-a)
 end

 local function grad(hash,x,y,z)
  local h=band(hash,15)
  local u,v,r

  if h<8 then u=x else u=y end
  if h<4 then v=y elseif h==12 or h==14 then v=x else v=z end
  if band(h,1)==0 then r=u else r=-u end
  if band(h,2)==0 then r=r+v else r=r-v end

  return r
 end

 function noise(x,y,z)
  y=y or 0
  z=z or 0

  local xi=band(x,255)
  local yi=band(y,255)
  local zi=band(z,255)

  x=band(x,0x0.ff)
  y=band(y,0x0.ff)
  z=band(z,0x0.ff)

  local u=f[x]
  local v=f[y]
  local w=f[z]

  local a =p[xi  ]+yi
  local aa=p[a   ]+zi
  local ab=p[a+1 ]+zi
  local b =p[xi+1]+yi
  local ba=p[b   ]+zi
  local bb=p[b+1 ]+zi

  return lerp(w,lerp(v,lerp(u,grad(p[aa  ],x  ,y  ,z  ),
                              grad(p[ba  ],x-1,y  ,z  )),
                       lerp(u,grad(p[ab  ],x  ,y-1,z  ),
                              grad(p[bb  ],x-1,y-1,z  ))),
                lerp(v,lerp(u,grad(p[aa+1],x  ,y  ,z-1),
                              grad(p[ba+1],x-1,y  ,z-1)),
                       lerp(u,grad(p[ab+1],x  ,y-1,z-1),
                              grad(p[bb+1],x-1,y-1,z-1))))
 end
-->8
-- music functions


rates={}
reverb={}
distorsion={}
filter={}
ch1={}
ch2={}
ch3={}
ch4={}
ti={0,0,0,0,0,0}

--sync keeps track of music
--and values related to it
--(also some redundant/deprec.
-- things)
function sync()
 b=15--rnd_efu-btn()
 pattern=stat(24)+1
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
 else
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

--[[
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
--]]

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

-->8
--raycast 


mw = 24
mh = 24
w = 128
h = 128
texw = 64.0
texh = 64.0

px = 56
py = 10.5

a = 0

dirx = -1
diry = 0

planex = 0
planey = 0.66

moved = 1

clrmode = 0
skew = 0


function raycast()

 local msx=planex
 local msy=planey
 
 local stx=dirx
 local sty=diry

	poke(0x5f38, 4)
	poke(0x5f39, 4)

 for y=64,128 do  
  local dist=(1024/(2*y-128+1))

  local dby4=dist/2
  local d16=dby4/64
  
  tline(
   0,y,128,y,
   (25+(px/0.25)+(-msx+stx)*dby4),
   ((py/0.25)+(-msy+sty)*dby4),
   d16*msx,d16*msy,0
  )
  end

	texw = 32
	texh = 32

	poke(0x5f38, flr(texw))
	poke(0x5f39, flr(texh))

 -- walls
	for x = 0, w, 1 do
		local camx = 2.0 * x / 128.0 - 1.0
		local rdx = dirx + planex * camx
		local rdy = diry + planey * camx
		
		local mx = flr(px)
		local my = flr(py)
		
		local sdx = 0
		local sdy = 0
		
		local ddx = abs(1.0 / rdx)
		local ddy = abs(1.0 / rdy)
		
		local pwalld = 0
		
		local stepx = 0
		local stepy = 0
		
		local hit = 0
		
		local side = 0
		
		if (rdx < 0) then
			stepx = -1
			sdx = (px - mx) * ddx
		else
			stepx = 1
			sdx = (mx + 1.0 - px) * ddx
		end

		if (rdy < 0) then
			stepy = -1
			sdy = (py - my) * ddy
		else
			stepy = 1
			sdy = (my + 1.0 - py) * ddy
		end
		
		while(1==1) do
			if (hit == 1) then break end
			if (sdx < sdy) then
				sdx += ddx
				mx += stepx
				side = 0
			else
				sdy += ddy
				my += stepy
				side = 1
			end
			mt = mget(mx,my)
			if(mt == 0) then hit = 0
			elseif(mt == 20 and sdx < 2) then hit = 2
			else hit = 1 end
		end -- dda
	
		if (side == 0) then
			pwalld = (mx - px + (1.0 - stepx) / 2.0) / rdx
		else
			pwalld = (my - py + (1.0 - stepy) / 2.0) / rdy
		end

		texnum = mget(mx,my)
		hiwall = 0

		if (texnum >= 19) then texnum = texnum-12 hiwall = 1 end

		lineh = flr(h / pwalld)
		
		draws = flr(-lineh / (2-hiwall) + h / 2)
		if (draws < 0) then draws = 0 end
		drawe = flr(lineh / 2 + h / 2)-1
		if (drawe >= h) then drawe = h-1 end
	
		
		wallx = 0.0
		
		if (side == 0) then
			wallx = py + pwalld * rdy
		else
			wallx = px + pwalld * rdx
		end
		
		wallx -= flr(wallx)
		
		u = flr(wallx * texw)
		if (side == 0 and rdx > 0) then
			u = texw - u - 1
		end

		if (side == 1 and rdy < 0) then
			u = texw - u - 1 
		end
		
		pstep = (1.0 * texh) / lineh
		
		texpos = (draws - 63 + lineh / 2.0) * pstep
		v = texpos

 	palt(0,false)

		if (side == 2) then
			pal(1,0)
			pal(2,0)
			pal(3,0)
			pal(4,1)
			pal(5,0)
			pal(6,5)
			pal(7,6)
			pal(8,2)
			pal(9,2)
			pal(10,4)
			pal(11,3)
			pal(12,5)
			pal(13,1)
			pal(14,4)
			pal(15,4)
		end

		tline(x,draws,x,drawe,8+8*flr(texnum/5)+rotr(u,3),((texnum-1)%4)*8+rotr(v,3),0,rotr(pstep,3),0)
		zbuf[x] = pwalld

		pal()
	end -- wall
	
	--sprites
		i = 1
	for s in all(sp) do
		spord[i] = i
		spdist[i] = ((px - s.x) * (px - s.x) + (py - s.y) * (py - s.y))
		i+=1
	end

	ce_heap_sort(sp,px,py)	

	i = 1
	for s in all(sp) do
		sx = sp[spord[i]].x - px
		sy = sp[spord[i]].y - py
		
		invd = 1.0 / (planex * diry - dirx * planey)
	
		tx = invd * (diry * sx - dirx * sy)
		ty = invd * (-planey * sx + planex * sy)

		ssx = flr((w/2) * (1+tx/ty))
			
 	--h
		sprh = abs(flr(h/ty))
		
		dsy = -sprh / 2 + h / 2
		if (dsy < 1) then dsy = 1 end
		dey = sprh / 2 + h / 2
		if (dey >= h) then dey = h-1 end
		
		--w
		sprw = abs(flr(h/ty))
		dsx = -sprw / 2 + ssx		
		dex = sprw / 2 + ssx
		if (dex >= w) then dex = w-1 end

		zs = flr(abs(dsx+8))
		ze = flr(abs(dex-8))
		
		if (ze >= w) ze = w-1
		if (zs >= w) zs = w-1

		if (ze == 0) ze = 1
		if (zs == 0) zs = 1
		
		if (ty > 0 and ty < zbuf[zs] and ty < zbuf[ze]) then
			sspr(s.s_x,s.s_y,s.s_w,s.s_h,dsx,dsy,sprw,sprh)
		end
			
		i+=1
	end
	-- sprites
end

function sky()

	rectfill(0,0,128,8,7)

	rectfill(0,8+16,128,8+24,2)

	for x = -50, 50 do
	map(36,0, a*140+x*4*8,8, 4,2)
	map(36,2, a*80+x*4*8,8+24, 4,2)
	map(36,4, a*40+x*4*8,8+40, 4,2)
	end

end

-->8
--effects

function efu_smear()
 local i
 for i=1,127 do
  smcpy(0x6000+i*63-rnd(2+(stat(24)%6)*0.25),0x6000+i*63-rnd(2+(stat(24)%8)*0.25),rnd(128)) 
 end 
end

function efu_border()
 rectfill(1,0,3,128,13)
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
 poke(0x5f5e,0b11111001)
 --cls(bg)
 rectfill(0,0,128,128,3)
 poke(0x5f5e,0b11111110)
 if pattern>8 then 
  efu_smear()
 end
 for i=1,#pox do
  ii=i+1
  if ii>#pox then ii=1 end
  iii=ii+1
  if iii>#pox then iii=1 end
  dx[1]=(pox[i]*(fv/poz[i])+64)
  dy[1]=(poy[i]*(fv/poz[i])+64)
  dx[2]=(pox[ii]*(fv/poz[ii])+64)
  dy[2]=(poy[ii]*(fv/poz[ii])+64)
  dx[3]=(pox[iii]*(fv/poz[iii])+64)
  dy[3]=(poy[iii]*(fv/poz[iii])+64)
  line(dx[1],dy[1],dx[2],dy[2],8+rnd(2)) 
  line(dx[2],dy[2],dx[3],dy[3],11-rnd(2))
 end
 if hit[2]>35 then
  sc=640
  rand_poly(-100,-100,50,100,100,250)
 end
 efu_wiggle(1.5)
 yield()
end

-- xor twisting and stuff
-- needs optimisation
function efu_xor()
	local mem
	local c
	local b=efv[2]
	cls(bg)
	for mem=0,0x1fff do
  c=efv[2]&((mem%64)^^flr(mem>>>7))*0.4+t*30
 	poke(mem+0x6000,((c<<4)|c)) 
	end
	yield()
end


--horisontal screen wiggling
function efu_wiggle(str)
 for i=1,127 do
  smcpy(0x6000+i*63+sin(tt+i*(0.02*hit[4]))*(note[3]/18.)*str,0x6000+i*63,64) 
 end
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
 redpal()
end

precalc = 0
ff = 0
	phi = 0.0

nohh = {}

function voxel()
	cls()

	-- torus
	c = 4.0
	local a = 1
	pi = 3.1415

	ii = 0
	nn = 0

	for y = 0, 64 do
		for x = 0, 64 do
			if precalc == 0 then
		 xx = (x/64)*0.03
		 yy = (y/64)*0.2
			xt = (c+a*cos(2*pi*yy))*cos(2*pi*xx)
			yt = (c+a*cos(2*pi*yy))*sin(2*pi*xx)
			zt = a*sin(2*pi*yy)
			nn = abs(noise(xt,yt,zt)*21)
			nohh[ii] = nn
			else
			nn = nohh[ii]
			mset(x+64,y,nn)
			end

			ii+=1
		end
	end


	ff+=1
	if (ff > 1) then ff = 0 end

	eh = 50
	sc = 50
	dist = 155.0

	py=-time()*40
	px=-cos(time()*0.1)*15

	phi = 0.07+cos(time()*0.01)*0.5	

	sinphi = sin(phi)
	cosphi = cos(phi)
	precalc = 0

	ii = 1
	z = dist
	zz = 2.0
	for ii = 1, 256 do

	hori = abs(100-abs(cos(time()*1+ii*0.001)*time()*5))

		z -= zz
		if (z < 20) then break end
		zz = z*0.02


		if(precalc==0) then
			lx = (-cosphi*z - sinphi*z) + px
			ly = (sinphi*z - cosphi*z) + py
	
			rx = (cosphi*z - sinphi*z) + px
			ry = (-sinphi*z - cosphi*z) + py
	
			dx = rotr(rx-lx,7)
		
			lxi[ii] = lx	
			rxi[ii] = rx	
			lyi[ii] = ly	
			ryi[ii] = ry	
		else
		
			lx = lxi[ii]+px
			rx = rxi[ii]+px
			ly = lyi[ii]+py
			ry = ryi[ii]+py
			dx = rotr(rx-lx,7)
		end
		
		
	
		ii += 1
		
		for i = 0, 127,2 do
			mt = mget(64+(lx&63),ly&63)

			hs = (eh - mt) / z * sc + hori
			mt-=flr(z*0.06)
			if (mt<0) then mt = 0 end
			rectfill(i,hs,i+1,128-z*0.5,mt)
			lx += dx*2
		end
	end
	rectfill(0,128-17,128,128,0)
	
	voxelpal()
	precalc = 1
end 
-->8
--palettes


function voxelpal()
	poke4(0x5f10,0x8582.8000)
 poke4(0x5f14,0x8d05.8483)
 poke4(0x5f18,0x8f86.0d8c)
 poke4(0x5f1c,0x0787.0f06)
 poke(0x5f2e,1)
end
 
function casterpal()
	poke4(0x5f10,0x0706.0580)
 poke4(0x5f14,0x0d0c.8c01)
 poke4(0x5f18,0x0989.0484)
 poke4(0x5f1c,0x8a8b.0383)
 poke(0x5f2e,1)
end

function redpal()
 poke4(0x5f10,0x8582.8000)
 poke4(0x5f14,0x0706.8605)
 poke4(0x5f18,0x088e.0987)
 poke4(0x5f1c,0x8084.0288)
 poke(0x5f2e,1)
end

-- call this in _draw to 
-- see the current palette
-- as a list of colors
function debug_palette()
 rectfill(0,0,128,128,0)
 for i=0,15 do
  print(@(0x5f10+i),0,6*i,i)
  
  print(i,4*4,6*i,i)
 end
end 
__gfx__
00000000333333330000089aa9b9b9b900000000f3dddcff33323333333233213333233373333321333333333333332122322222223232222272222227222322
000000003332233303fffff99a9b9b9b46555550ff3dddcf22222222222322272222322232222227222222222222222777273377772727333333773371777277
0000000033711733032ded38899999a943665650fff3dddc222222222222222722222222222222272222222222222227131bbb333331122333221bbbb1333311
00000000321001230277d2309899999a43666550cfff3ddd222222222212222722222222222222272223333333333227331aabbbbbb1222222201baab1223331
0000000032100123071f7720899999a943366650dcfff3dd2222222222722227222222222222222722733baaaab3322722300abbba0172722701ba99a1022231
000000003371173341fef1709899999a43636650ddcfff3d272222222222222722222222222222272273baaaaaab3227022319aaa01107770011000001102721
00000000333223335fedef108989899943333360dddcfff3232222222222222722222221222222272273aa3333aa3227072218990111100011b3baaaa9117271
00000000333333336540000098989899444444403dddcfff222222222222222722222227222272272273aa3333aa3227107010001121189aabbbbbbbb0117771
000000000000000000000000b3333333b3333333abbbbbbb222222222222222722222222222232272273aa3aa33a3227110111aa122210889bbbbbb991117771
000000000023320000000000abbbbbb3abbbbbb39aaaaaab222222222222722722222222222222272273aa33aa33322713311bbba07711008899998881117101
00000000023a300000000000abbbbbb3abbbbbb39aaaaaab222222222222322722222222222222272273baa33aa33227bbb1ba90010011110088880001110011
0000007323a3000000000000aaaaaaabaaaaaaab9999999a2222272222222227222222222222222722733baa33aa3227aaa1a9011111b3b3b100002223311111
0000073333300000000000003333b333bbbbabbbbbbbabbb222223222222222722272222222222272273333333333227a991901333110bbbbb11172722211331
000073247370000000733000bbb3abbbaaab9aaaaaab9aaa2222222222222227222322222222222722777777777722279901012333311aaaaaa112727201bbb1
000033742333700073332200bbb3abbbaaab9aaaaaab9aaa2222222222222227222222222222222722222222222222270011113232311aaa99911727201baaa1
000093333333333333322000aaabaaaa999a9999999a9999222222222222222722222222222222272222222222222227111172722321199990011077711aa991
000002223333333333320000abbbbbbb9aaaaaaa9aaaaaaa22222222227222272222222222222227000002000000200033112727722111111111111111109001
0000003333333333333000009aaaaaab8999999a8999999a22212222223222272222227222222227000002200007220023317277277111111133333311110113
0000733222222222233300009aaaaaab8999999a8999999a22272222222222272222223222222227000002270002ab0022017700700133311abbbbbbb1111122
0003332707222227722330009999999a888888898888888922222222222222272222222222222227000002220072b300001100110113233310abbbbbbba11100
003322000000000000223300aaaa9aaaaaaa9aaa999989992222222222227227222222222227222700000722702ab0001333311111100232320abbaaaaaa1133
00020000300ab9ba00002300999a8999999a8999888908882222222222223227222222222223222700000022272b300022223111133b10232310aaaaaa981122
00000000300bbbbb00000300999a8999999a8999888908882222222222222227222222222222222700000023333200002722011abbbb11222221099998001727
000000003009bbb900000000888988888889888800080000777777777777777177777777777777710000033333330000727011aaaabb11272221189800727272
0000000030008b80000000008999999989999999088888882222272222222227222222222222222700000333327320000001119aaaaaa1727271100011002700
00000000000000000000000008888889088888890000000877777277777777717771777777777771000023332473333211111189aaa901072721111111110011
033703030337033703030730088888890888888900000008222222222222222722232222222222270000333374233333333b11009900111007711111133b1b33
030303030303030303730220000000080000000800000000777777777777777177777777077777710002333322333333bbbba111001b33b110012311bbbbbbbb
033703030303030302320370999989998888088888880888777777777777777177777777177777710002893333333333aaaaa9111aabbbbaa1123211aaaaaaba
0303037303030303003000008889088800080000000800007c7c7c7c7c7c7c747c7c7c7c7c7c7c7400021233333233339e999811aaabbeaaa11322e100899aea
033202320303030300300370888908880008000000080000c7c7c7c7c7c7c7c1c7c7c7c7c7c7c7c100002222222333330e88e111e9aeaeaae17e72ee7e0e89e0
000000000000000000000000000800000000000000000000ecedecededecede4edecededecedece40000727772233333ecedecdefedeefdeceddeedfedecedfd
cedcedcedeeedeedecdceecdcecdcdec898a999a0809098989a9999a088888897222222222222223722222222222222322222223333333333333333332222222
eecdcdedcdedcdeecededdedecdeedde080988890909888900988889088889891777777777777772177777777777777277777772222222222222222227777777
eddeeefcedeceeddcecfed8dec8cddce009888890980988900988989009809891777777777771072171077777777777211111117777777777777777771111111
dceededcdefdedededdeecdeeedeede8009889890098809908090989080980991777777777770172170177777777777211111111111111111111111111111111
d8cdcdcdcdedccdeededceeedddcfcec009809890888809909880989080980991777777777770872177777777777777210101010000000000000000010101010
deedefedeedcdecececeeedcdefdedee088809890098009909880989098880991777777777778972177772222223777201010100666666666666666601010101
cedeeeddcdedefde8eedfcdeceddeedc088880990098098900909889098888891777777777779772177700000017777200000000333333333333333300000000
dcd8decfede8eeddecdeeddedeececce088880990000000808888899000000081777777777777772177772222223777201010100111111111111111101010101
ceceddceedcdedceceeddeecedceddec0888809989998a9a098880998999999a1777777777777772177700000017777200000000222222222222222200000000
eecefededcedccecedceec8edeece8ee088980990888809908898889088888891777777777777772177772222223777200000002333333333333333320000000
cecdededcefededddedeceddedefdedc080980890809809908099889088888991777777777777772177700000017777200000002333333333333333320000000
dddcdd8edcdcdeeddcdfedcdd3cdcefd089988890809888908809889089880991777777777777772177777777777777200000002333333333333333320000000
deefdedeeeee8ccdeeceedde3b3eedee089888890098098908909889009880991710777777777772177777777777107200000017232333333333232320000000
eddeceddcdcddfecedfec8ece3dedede088889890098098900988889009809991701777777777772177777777777017200000002323232323232323271000000
eecdcddeeedededccddcecededececde088809890888809900980989088809891708777777777772177777777777777200000007232323232323232320000000
cdceedeedccecdcddeeceecdceddeee8000008080809888900000008088809890111111111111117011111111111111700000017222232323232222271000000
77772222277727777777777777777777222222227272222222233222277272222222222222222222222222222222222200000017222222222222222271000000
77777272777772777777777777772727222222272727222222222322277727222222222222222222222222222222222200000017222222222222222271100000
77777727277777777777722777272777722222227222222222222233227772722222772222222277222222227777772200000171727272222272727271000000
77777777777777727272227777777777772727272722222222272222272777772777777222777777722277777777777700000017272727272727272717100000
27277772727777772727772777777777777272222222222272727722227272777777777777777777777777777777777700000172727272727272727272100000
72777777772727777777777777777772272722222222222227272722222222727777777777777777777777777777777700000177777727272727777777100000
77777777777777777777777777777777222222222233322222222222222222227722277777727777777777777722777700000171717777777777777171100000
77777777777777777777777777777777322222222233333222232222222333337422227775222777777452777452227700000117171717171717171717100000
77777777222277777222222277777777332222222333333333332223222222334452227444522277777452774445222700000111117171717171717111110000
77772222222222222277722222277777222223233333333333322233322222224445222444452277774452774444522200001010111111111111111110100000
22222772727222277777772272222222222333333333333333333333332233224444444454445227444522744444455200000101010101010101010101010000
22222227222222222722777777227722333333333333333333333333333333334544445554445224444444444444444400000000101010101010101010000000
22222222222222222272727727222222333333333333333333333333333333335454444555444524454544555544444400000000000101010101000000000000
22222222232333232222222272222222333333333333333333333333333333334545444444444444444444455444444400000000000000000000000000000000
22323222223232323322222222223222333223333333333333333333333333334444444444444444444444444444444401010101010101010101010101010101
23232222222222222322222222232222332222222333222222222233332233334444444444444444444444444444444477777777777777777777777777777777
271711100ddddddddddddddddddddddd000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
333333270ddddddddddddddddddddddd000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
333333270ddddddddddddddddddddddd000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
333333270ddddddddddddddddddddddd000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
327277710ddddddddddddddd0ddddddd000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
3333332710dddddddddddddd000ddddd000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
3333332770dddddddddddddd2000dddd000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
33333327270ddddddddddddd32200ddd000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
33333327230ddddddddddddd33320ddd000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
327277712320dddddddddddd313320dd000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
3333332723320ddddddddddd333320dd000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
333333272333200dddddddddbbb3320d000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
33333327233333200ddddddd33bb320d000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
3333332723333333200ddddd33333320000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
327277712333333333200ddd31033320000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
33333327233333333333200d33333320000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
3333332723333333333333200ddddddd000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
3333332723333338b8888333200ddddd000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
2717111023333338888883333320dddd000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
3331032723333333333333333320dddd000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0333332000000000010000000000dddd000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0333327123333333323333333320dddd000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
033327372333333332320000000ddddd000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000337233200000100dddddddddddd000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000003370000dddddddddddddddddddd000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00177710dddddddddddddddddddddddd000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0d00000ddddddddddddddddddddddddd000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1dd010dddddddddddddddddddddddddd000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0d17271ddddddddddddddddddddddddd000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01723271dddddddddddddddddddddddd000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
07233327dddddddddddddddddddddddd000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0ddddddddddddddddddddddddddddddd000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__label__
dddgdddgggggddggddggdddgdddgggggggggggggdddgggggdddgdddggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
kkkgdgdggggggdgggdggdgdgggdggggggdggggggdgdgggggdgggggdggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
kdkgdddggggggdgggdggdddgdddgggggggggggggdgdgggggdddgdddggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
kgkgdgdggggggdgggdggdgdgdggggggggdggggggdgdgggggggdgdggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
kdkgdddggdggdddgdddgdddgdddgggggggggggggdddggdggdddgdddggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
kkkggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggss
ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggssssssss
gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggsssssssggsssggsss
gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggssssssggssgggssggsssggsss
gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggsssssgggssggssggsggggssggssggggss
ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggssssggssggssgggsggggsggsgggggsggssggggss
ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggsssssggsggssggsggggsgggsggggsggsgggggsggsssggsss
ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggsssssggssggssggsgggsggsggggsgggsggggsggssgggssggsssggsss
gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggsssssggssggsggsggggsggsgggsggsggggsgggssggssggssssgssggssssssgg
gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggssggsggsgggsggsggggsggsgggsggssggssgggssssssggsssssssgggggggggg
gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggsggggggsgggsggsggggsggsggssggssssssgggssssssggggggggggggggggggg
gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggsggggggssggsggssggssggsssssggsgggggggggggggggggggggggggggggggss
gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggssggsggssggsggssssssggsggggggggggggggggggggggggg66gggggssssssss
gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggsssssggsssssggsggggggggggggggggggggggggg6ggggggg66gggggsssggsss
ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggssgggggggggggggggggggggggggg6gggggg6g66gggg6g666g6ggsssggsss
ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg6gggggg6ggggggg66g6ggg6g66g6ggg6ggg6gggssggggss
gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg6gggggg6gggggg66g6gg66gg66gggg6gg6gggg6ggg6gggssggggss
gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggsssssgg6g666gg6g66g6gg6gg6gggg6gg6ggggg6gg6gggg6ggg6gggsssggsss
gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggssggsggg6gg6gg66gg6gggggg6gggg6gg6gggg6g666ggg6g666g6ggsssggsss
5ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggsggggggg6gggggg6gg6gggggg6ggggg66g6ggg6g66g6gggggg6ggggssssssgg
6cc55ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggsggggggg6ggggggg66g6gg666g6gg6gg6gggggggg6gggggggg6gggggggggggg
cccs6cc66ggggggggggggggggggggggggggggggggggggggggggggggggggggggggssggggg6g666gg6gg6g6gg6g6gggggggggggggggggggggggggggggggggggggg
cccs6cccc5633sgggggggggggggggggggggggggggggggggggggggggggggggggggsssgsggggg6gggggg6gggggggggggggggggggggggggggggggggggggggggggss
sss56ccccc633s5cc5gggggggggggggggggggggggggggggggggggggggggggggggsssssggggggggggggggggggggggggggggggggggggggggggg66gggggssssssss
55555ssccc633s5cc5cc65ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggssssgggsssssgggssssssgg6g66gggggsssggsss
c655555sss633s6cc5ccc56335gggggggggggggggggggggggggggggggggggggggggggggggg6gggggsssssggsssssggssggssgggssggssgg6g666g6ggsssggsss
cccs633665633s5sc5cccc6335c566gggggggggggggggggggggggggggggggggggsssssgggg6ggggssggssggsggssggssggssgggsggggsggg6ggg6gggssggggss
cccs633335633s565555ss633cc5ccc635gggggggggggggggggggggggggggggggssggsgg6g666ggsggggsggsgggsggsggggsgggsggggsggg6ggg6gggssggggss
ss5563333365356cc56655633sc5ccc635c5ccgggggggggggggggggggggggggggsggggggg6gggggsggggsggsgggsggsggggsgggsggggsgg6g66g6gggsssggsss
55ss533333s53s6cc53335633655ss5635c5cc535cgggggggggggggggggggggggsggggggg6gggggsggggsggsggssggssggssgggssggssgg6g666g6ggsssggsss
655s555ss556555sc53333535cc566363555ss53cccc3scggggggggggggggggggssggggg6g666ggssggssggsggssggssssssgggssssssgggggg6ggggssssssgg
333s5555556cs655ss3335s55cc5333635c56653s5cc3sccc36ggggggggggggggssggsggggg66ggssssssggsssssggssssssgggggggssggggggggggggggggggg
3335c56cc56cs3335s55s556sss5333555c53353cc553s5cc3ccc3cggggggggggsssssgggggggggsggggsggsgggggggggggggggggggggggggggggggggggggggg
3335cs6ccc6cs3333566656c565sss5655s53355cc3355c6635cc3cc36cggggggggggggggggggggggggggggg5555ggg5555555ggggggggggggggggggssggggss
3335cs6ccc6cs33336s6cc6c633556c6s35s5555s5335sc333c663653cc3cc5ggggggggggggggggggggggg55gggg555gggg555gssssssgggg66gggggssssssss
3335cs6ccc6cs33336c6cc6c6336scc6s3355c5s3555c5s335s333c356635c6c3sssssgggg6ggggssssssg5ggggg55gggggg55gssggssgg6g666g6ggsssggsss
3335cs6ccc6cs33336c6cc6c6336ccc6s336sc5s3s5cc3356c65s5535c35c3633ssgssgg6g666ggssggssg5ggggg55gggggg55gssggssggg6gg6g6ggsssggsss
3335c56ccc6c533336c6cc6c6336ccc6s336sc5s3sscc33scc36cs35c656s353ssgggsggg6gg6ggsggggsg5ggggg55gggggg55gsggggsggg6ggg6gggssggggss
s555c56ccc55533336s6cc656336ccc65336sc5s3sscc33ccc3ccs35c35c3c366sggggggg6gggggsggggsg5ggggg55gggggg55gsggggsggg6ggg6gggssggggss
5665c55cccsss55556s6cc555ss6sccs5ss65c55355cc33ccc3ccs35c35c3c366sgggggg6g66gggssggssg5ggggg55gggggg55gssgggsgg6g666g6ggsssggsss
5cc5c5555555555666s5s55sss66ss55ss565s5ss55cs55scs5cc5s5535c3c365ssggsgg6g666ggssggssg5ggggg55gggggg55gssssssgggggg6g6ggsssggsss
5cc5c55666666666c6s5666665c6s533666656665s5666cs555c55c5555s6s555sssssggggg6gggssssssg5ggggg55gggggg55gssssssgggggg6ggggssssssss
5cc5555555333556c555553335c55556356555635s5535c5535553c53553c5563ggggggggggggggggggggg5555555555555555gggggggggggggggggggggggggg
5cc5566665633s56c566656335c566c635c5cc536ccc3sccc3ccc3cc3cc3cc5c3ggggggggggggggggggggg5555555555555555gggggggggggggggggggggggggg
cccs6ccccc633s5cc5cccc6336c5ccc635c5cc53cscc3sccc3scc3s5355355533sssgggggg6gggggssssgg55gggg555gggg555gssssgggggg66gggggssggggss
cccs6ccccc633s6cc5cccc633cc5cc563555555355553s56636663c35c35c363sssgssgg6g66gggssggssg5ggggg55gggggg55gssssssgggg66gggggssssssss
css56sssss633s5ss55555633555553635c53353cc3355c333c335c35s36s5666sgggsgg6g666ggssggssg5ggggg55gggggg55gssggssgg6g666g6ggsssggsss
5555555555633s56556665633cc5333635c53355c53355533655ss55c35c3c366sggggggg6gggggsggggsg5ggggg55gggggg55gsggggsggg6gg6g6ggsssggsss
665553333563356cc53333535cc5333555s5ss556s55c3556c36cs35c35c3c365sggggggg6gggggsggggsg5ggggg55gggggg55gsggggsggg6ggg6gggssggggss
cccs633333653s6cc53333s5scs53356s65s565s355cc33scc3ccs35c35c3cs55ssggsgg6g666ggssggssg5ggggg55gggggg55gssgggsggg6ggg6gggssggggss
cccs633333s55s5s55333556555s55c6s3355c5s3sscc33ccc3cc535555s66611sssssggggg66ggssssssg5ggggg55gggggg55gssggssgg6g666g6ggsssggsss
cs5s533335565555ss55556c63356cc6s336sc5s3sscc33cc53ccs555553ccsgggggssgggggggggsggggsg5ggggg55gggggg55gssssssgggggg6g6ggsssggsss
s5sss555556cs6335566c56c6336ccc6s336sc5s3s5cc5sscs5c56c53ssggggggggggggggggggggggggggg5555555555555555gggggssgggggg6ggggssssssss
655s5556656cs33336s6cc6c6336ccc6s3365c55s55css6s53555csgggggssgscsssgggggggggggggggggggggggg555gggg555gggggggggggggggggggggggggg
3335656ccc6cs33336c6cc6c6336ccc653s65c5s5s5663c55csggggggggggggggssgssgggg6ggggssssssggsssssgggssssggggggggggggggggggggggggggggg
3335cs6ccc6cs33336c6cc6c6336sccss5565566ss553ssggggggggggggggggggssggsgg6g666ggssggssggsggssggssssssgggssssssgggg66gggggssgggggg
3335cs6ccc6cs33336c6cc6553s6sc5555665363cggggggggggggggggggggggggsggggggg6gg6ggsggggsggsggssggssggssgggssggssgg6g66gggggssssssss
3335cs6ccc6cs33336s6cc55s566s5333365cssscccggggccsscccgcccccgggsgsggggggg6gggggsggggsggsgggsggsggggsgggsgggssgg6g666g6ggsssggsss
3335cs6ccc6c5333s6s6cc5s55c65311csggggggggggggggssssccccccssscgggssggggg6g66gggsggggsggsgggsggsggggsgggsggggsggg6ggg6gggsssggsss
3335c56ccc55555566s555663sc5ggggggggggggggggggggggggggggggggggggssssgsggggg66ggssggssggsggssggssggssgggsggggsggg6ggg6gggssggggss
3335c56cccssss56c6s53333ssggggggggggggggggggggggggggggggggggggggggggssggggg6gggssssssggsssssggssggssgggssgggsgg6g66g6gggssggggss
s555c55ss5555566c655ssggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggsggsssssggssssssgggssssssgg6g666g6ggsssggsss
5665c55666663356ssgggggggggggggggggggggggggggggggggggggggggggggggsssgggggggggggggggggggggggggggggggsgggggggssgggggg6ggggsssggsss
5cc5c5533533ssgggggggggggggggggggggggggggggggggggggggggggggggggsssssssgggg6gggggg6ggggggggggggggggggggggggggggggggg6ggggggssssss
5cc5c55ssggggggggggggggggggggggggggggggggggggggggggggggggggggssccssggsgg6g66ggggg6gggggg6ggggggggggggggggggggggggggggggggggggggg
5ccgggggsgggggggggggggggggggggggggggggggggggggggggggggggggggggssssggggggg6gg6gg6g66g6gg666g6gggg6gggggggg6gggggggggggggggggggggg
ssssccccccccssssssggggggggggggggggggggggggggggggggggggggggsscccccsggggggg6gggggg6gg66gg6gg6ggg6g66ggggg6g66gggggg66gggggssgggggg
ccgggggggcccccssssscccsssssssgggggggggggggggggggggggggggsscccggggssgggggg6gggggg6gg6gggggg6gggg6gg66ggg6g66g6gg6g66gggggssssssss
cgggggggcccssssscccccgggcccccccsggggsssgggggggggggggggsssccccggggsssgsgg6g666gg6g66ggggggg6gggg6gg6ggggg6gg6gggg6gg6g6ggsssggsss
cccccccssssssccgggggggggggcccsssssccccccccsssssssggggggssccccccccgggssggggg66gg6gg6g6gg666g6ggg6gg6ggggg6gg6gggg6ggg6gggsssggsss
ssscsssssccccccggggggggcccssssssccgccggggggcccccsssgggsssssssssssggggggggggggggggg6gggggg6gggg6g66g6ggg6g666gggg6ggg6gggssggggss
ggggggsssssccccccccccccsssssscccgggggggggggcccsssssscccccccccssssgggggggggggggggggggggggggggggggg6g6ggg6g66g6gg6g66g6gggssggggss
gggggggggggssssssssccssssssccccccggggggggcccsssssscccgccggggggcccsssssggssssgggggggggggggggggggggggggggggg6gggggggg6g6ggsssggsss
ggggggggggggggggggggggggsssccccccccccccccccsssssscccgggggggggggccssgssggssggsggsssssggggggggggggggggggggggggggggggg6ggggsssggsss
ggggggggggggggggggggggggggsssssssssccccccssssssccccccggggggggggccsgggsggssggsggssggssggsssssggggggggggggggggggggggggggggggssssss
ggggggggggggggggggggggggggggggggggsssssggggssccccccccggggggccccccsggggggsgggsggsggggsggsggssggsssssggggssssggggggggggggggggggggg
ggggggggggggggggggggggggggggggggggggggggggggssssssscccccccccccccsssgggggsgggsggsggggsggsgggsggssggssgggssssssggssssggggggggggggg
ggggggggggggggggggggggggggggggggggggggggggsssssssssssssssssccccsssssgsggssggsggsggggsggsgggsggsggggsgggssggssggsssssssggssgggggg
gggggggggggggggggggggggggggggggggggggssssccccccsssssssssssssssggggggssggsssssggssggssggsgggsggsggggsgggsgggssggssgggssggssssssss
gggggggggggggggggggggggggggggggggggsssscccccccccccccccsssssssgggggggggggggggsggssssssggsggssggsggggsgggsggggsggsggggssggsssggsss
ggggggggggggggggggggggggggggggggggsssccccccccggggggcccccccccsgggggggggggssssggggggggsggsssssggssggssgggsggggsggsgggggsggsssggsss
ggggggggggggggggggggggggggggggggssssccccgggggggggggccccccccssssgggggggggggggggggssssggggggggggssssssgggssgggsggsgggggsggssggggss
ggggggggggggggggggggggggggggggssssccccgggggggggggggggcccccssssggggggggggggggggggggggggggssgggggggggsgggssssssggssgggssggssggggss
ggggggggggggggggggggggggggggsssscccccggggggggggggggggccccssssggggggggggggggggggggggggggggggggggggssggggggggssggssssgssggsssggsss
gggggggggggggggggggggggggggssssccccccccgggggggggggggccccssssgggggggggggggggggggggggggggggggggggggggggggssssggggggggsssggsssggsss
gggggggggggggggggggggggggsssscccccccccggggggggcccggccccssssggggggggggggggggggggggggggggggggggggggggggggggggggggssssgggggggssssss
ggggggggggggggggggggggggggggsscccccccccccccggcccccccccssssgggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggsssgggggg
ggggggggggggggggggggggggggsssssssssscccccccccccccccccssssggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
ggggggggggggggggggggggggssssssssssssssssscccccccccccssssgggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
ggggggggggggggggggssgggssssssssssssssssssssssssccccssssggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
ggggggggggggggggsssssccccsssssssssssssssssssssssssgggsgggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
gggggggggggggggsssssccccccccccsssssssssssssssssssggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
gggggggggggggssssscccccccccccccccccsssssssssssssgggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
gggggggggggsssssccccccccccccccccccccccccsssssssggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
gggggggggssssssccccccccccgggggcccccccccccccccsgggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
ggggggggssssscccccggccccggggggggggcccccccccccsssssgggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
ggggggsssssccccccggggggggggggggggcccccccccccsssssggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
ggggsssssscccccgggggggggggggggggcccccccccccsssssgggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
gggsssssccccccggggggggggggggggggggccccccccsssssggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
gsssssscccccgggggggggggggggggggggggccccccsssssgggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
sssssccccccgggggggggggggggggggggggcccccssssssggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
sssccccccccccggggggggggggggggggggcccccssssssgggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
ssccccccccccccgggggggggggggggggccccccssssssggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
ccccccccccccggggggggggggggggggccccccssssssgggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
cccccccccccggggggggggggccggggccccccsssssssgggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
cccccccccccgggggggggggccccccccccccsssssssggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
ccccccccccccccgggggggccccccccccccsssssssgggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg

__gff__
0102020202020000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__map__
40414041000000000c0d0e0f0c0d0e0f05050505050505050000000000000000000000006061626301010101010101010101010101010101010101010101010100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
50515051101112001c1d1e1f1c1d1e1f05050505050505050000000000000000000000007071727301000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
42434041202122002c2d2e2f2c2d2e2f05050202020205050000000000000000000000006465666701000000000101000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
52535051303132003c3d3e3f3c3d3e3f05050202020205050000000000000000000000007475767701000000000101000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000c0d0e0f0c0d0e0f050502020202050500000000000000000000000068696a6b01000000000101000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000001c1d1e1f1c1d1e1f050502020202050500000000000000000000000078797a7b01000000000101000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000002c2d2e2f2c2d2e2f05050505050505050000000000000000000000000000000001000000000101000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000003c3d3e3f3c3d3e3f05050505050505050000000000000000000000000000000001000000000101000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000006161616161616161080908090a0b0a0b0000000000000000000000000000000001000000000101000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000006262626262626262181918191a1b1a1b0000000000000000000000000000000001000000000000000013001300130000000000000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000006363636363636363282928290a0b00000000000000000000000000000000000001000000000000000014001400140000000000000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000006464646464646464383938391a1b00000000000000000000000000000000000001000000000000000013001300130000000000000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000006565656565656565080908090a0b00000000000000000000000000000000000001000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000006666666666666666181918191a1b00000000000000000000000000000000000001000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000006767676767676767060708090a0b00000000000000000000000000000000000001000004040404040404040000000000000000001300000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000006868686868686868161718191a1b00000000000000000000000000000000000001000004000000000000040000000000000000001300000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000003030303030303030a0b0a0b000000000000000000000000000000000000000001000004000000000000040000000000000000000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000003030303030303031a1b1a1b000000000000000000000000000000000000000001000000000000000000040000000000000000000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000030202020202020326272829000000000000000000000000000000000000000001000004000000050000040000000000000000000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000030202020202020336373839000000000000000000000000000000000000000001000004000000000000040000000000000000000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000030202020202020306070809000000000000000000000000000000000000000001000004000000000000040000000000000000000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000030303030303030300000000000000000000000000000000000000000000000001020202040404040404040000000001020304050000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000030303030303030306070809000000000000000000000000000000000000000001000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000030303030303030326272829000000000000000000000000000000000000000001010101010101010101010101010101010101010101010100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000010101010101010100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000010202020202020100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000010201010101020100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000010201040401020100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000010201040401020100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000010201010101020100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000010202020202020100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000010101010101010100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
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
01 0d1a0d22
00 0a1a0a23
00 0b1a0b24
00 0c1b0c25
00 0a1a0a22
00 0a1a0a23
00 131a1324
00 141b1425
00 151a0d22
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

