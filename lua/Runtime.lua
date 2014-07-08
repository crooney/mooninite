-- mooninite Lisp-1 -> Lua runtime
--

local function id (...) return ... end

local function thunk(value)
    return {function() return value end}
end

local function force(thunk)
    if type(thunk) ~= 'table' then return thunk end
    if thunk[2] then return thunk[2] end
    local t = thunk
    while type(t) == 'table' do
        t = t[1]
    end
    if type(thunk) == 'table' then
        while type(t) == 'function' do
            t = t()
        end
        thunk[2] = t
    end
    return t
end

---[[
    local t = thunk('plain val')
    assert(force(t) == 'plain val')
    assert(force(t) == 'plain val')
    local trace = 0
    t = thunk(function() trace = trace + 1; return 42 end)
    for i=1,3 do force(t) end
    assert(force(t) == 42)
    assert(trace == 1)
--]]

-- Math ops.

local function binop(f,...)
    local xs = {...}
    local r = xs[1] or 0
    for i = 2, #xs do
        r = f(r,xs[i])
    end
    return r
end

local function addi(...) return binop(function(x, y) return x + y end, ...) end
local function subt(...) return binop(function(x, y) return x - y end, ...) end
local function mult(...) return binop(function(x, y) return x * y end, ...) end
local function divi(...) return binop(function(x, y) return x / y end, ...) end
local function modu(...) return binop(function(x, y) return x % y end, ...) end
local function expo(...) return binop(function(x, y) return x ^ y end, ...) end

---[[
do
    print("testing math ops")
    assert(addi(1,2,3,4,5,6,7,8,9,10) == 55)
    assert(subt(100,10,20,30) == 40)
    assert(mult(3,4,5,6) == 360)
    assert(divi(360,6,5,4) == 3)
    assert(modu(21,5) == 1)
    assert(expo(2,3,2) == 64)
    print("done")
end
--]]

--   Collections.

local function lcons(car,cdr)
    return {car, cdr; list = true}
end

local function car(l) return l[1] end

local function cdr(l) return l[2] end

local function is_list(x) return type(x) == 'table' and x.list end

local function is_vec(x) return type(x) == 'table' and x.vec end

local function is_seq(x) return type(x) == 'table' and x.seq end

local function is_coll(x) return is_seq(x) or is_vec(x) or is_list(x) end

local function cons(first,rest)
    local function sub(x)
        if type(x) == 'function' then return x
        else return function() return x end
        end
    end
    local out = {}
    out.first = sub(first)
    out.rest = sub(rest)
    out.seq = true
    return out
end

local function bumper_seq(coll,f)
    if #coll == 0 then return nil end
    local i = 1
    local function bump() i = i + 1; return i; end
    local out = {}
    out.first = function() return f(coll,i) end
    out.rest = function()
        local j = bump()
        if j > #coll then return nil end
        return out
    end
    out.seq = true
    return out
end

local function seq(coll)
    local coll = coll
    if not coll then return nil end
    if is_seq(coll) then
        return coll
    elseif is_list(coll) then
        coll.first = function() return car(coll) end
        coll.rest = function() return seq(cdr(coll)) end
        coll.seq = true
        return coll
    elseif is_vec(coll) then
        return bumper_seq(coll,function(c,n) return c[n] end)
    elseif type(coll) == 'string' then
        return bumper_seq(coll,function(c,n) return c:sub(n,n) end)
    else
        error("seq called on non-collection.")
    end
end

local function first(s)
    return s.first()
end

local function rest(s)
    return s.rest()
end

local function show_seq(s)
    local s = seq(s)
    if not s then return nil end
    print(first(s))
    show_seq(rest(s))
end

local function vector(...)
    -- don't know why return {... , vec = true} doesn't work
    local out = { ... }
    out.vec = true
    return out
end

local function to_vec(s)
    if is_vec(s) then return s
    else local s = seq(s)
        local out = {}
        repeat
            out[#out + 1] = first(s)
            s = rest(s)
        until not s
        out.vec = true
        return out
    end
end

local function to_list(xs)
    -- TODO make work with seq in 1 pass.
    if is_list(xs) then
        return xs
    elseif is_vec(xs) then
        local out = nil
        for i = #xs, 1, -1 do
            out = lcons(xs[i],out)
        end
        return out;
    else
        return nil
    end
end

local function to_string(s)
    if type(s) == 'string' then return s
    else
        local s = seq(s)
        local out = ''
        repeat
            out = out .. first(s)
            s = rest(s)
        until not s
        return out
    end
end

local function list(...)
    return to_list(vector(...))
end

local function index(ind,l)
    if is_vec(l) then
        return l[ind]
    else
        local s = seq(l)
        if s then
            for i = 1, ind do
                s = rest(s)
            end
            return first(s)
        end
    end
    return nil
end

local function length(list)
    if is_vec(list) then
        return #list
    elseif (not list) then return 0 end
    local s,n = seq(list), 0
    repeat
        n = n + 1
        s = rest(s)
    until not s
    return n
end

local function append(l,r)
    local xs,ys = seq(l),seq(r)
    if xs and (not ys) then
        return xs
    elseif xs then
        return cons(function() return first(xs) end
                   ,function() return append(rest(xs),ys) end)
    elseif ys then
        return ys
    end
    return nil
end

local function lazy_cat(...)
    local ss = {...}
    if #ss == 1 then return seq(ss[1])
    else
        local s = ss[1]
        for i = 2, #ss do
            s = append(s,ss[i])
        end
        return s
    end
end

local concat = lazy_cat

local function take(n,s)
    local s = seq(s)
    if not s then return nil end
    return cons(function() return first(s) end
               ,function() if n > 1 then return take (n-1,rest(s))
                                    else return nil end
                end)
end

local function drop(n,s)
    local s = seq(s)
    if not s then return nil end
    if n > 1 then return drop(n-1,rest(s)) end
    return rest(s)
end

local function optional_times(f,...)
    local as = {...}
    if #as <= 1 then return f(as[1])
    else return take(as[1],f(as[2]))
    end
end

local function take_while(f,s)
    local s = seq(s)
    if not s then return nil end
    local tmp = first(s)
    if not f(tmp) then return nil end
    return cons(tmp, function() return take_while(f,rest(s)) end)
end

local function drop_while(f,s)
    local s,x = seq(s),nil
    while s do
        x = first(s)
        s = rest(s)
        if not f(x) then break end
    end
    return cons(x,s)
end

local function fold(f,init,s)
    local s = seq(s)
    if not s then return init end
    repeat
        init = f(init, first(s))
        s = rest(s)
    until not s
    return init
end

local function equals(l,r)
    if l == r then
        return true
    else
        local l,r = seq(l),seq(r)
        if (l and r) then
            return equals(first(l),first(r)) and equals(rest(l),rest(r))
        end
    end
    return nil
end

local function filter(f,s)
    local s = seq(s)
    if not s then return nil end
    local tmp = first(s)
    if not f(tmp) then return filter(f,rest(s)) end
    return cons(tmp,function() return filter(f,rest(s)) end)
end

local function map(f, ...)
    local s = lazy_cat(...)
    if not s then return nil end
    return cons(function() return f(first(s)) end
               ,function() return map(f,rest(s)) end)
end

local function replicate(...)
    local function f(x)
        return cons(x,function() return f(x) end)
    end
    return optional_times(f,...)
end

local function repeatedly(...)
    local function f(g)
        return cons(function() return g() end
                   ,function() return f(g) end)
    end
    return optional_times(f,...)
end

local function iterate(f,x)
    return cons(function() return x end
               ,function() return iterate(f,f(x)) end)
end

local function reverse(s)
    local s = seq(s)
    if not s then return nil end
    local out = nil
    repeat
        out = lcons(first(s), out)
        s = rest(s)
    until not s
    return seq(out)
end

local function flatten(s)
    local s = seq(s)
    if not s then return nil end
    local h = first(s)
    if is_coll(h) then
        -- obviously, not tail recursive
        return append(flatten(h),flatten(rest(s)))
    else
        return cons(h, function() return flatten(rest(s)) end)
    end
end

local function range(...)
    local function r(f,t,s)
        local i,out = f,{}
        out.first = function() return i end
        out.rest = function()
            i = i + s
            if i > t then return nil end
            return out
        end
        out.seq = true
        return out
    end
    local as = {...}
    if #as == 0 then return r(0,math.huge,1)
    elseif #as == 1 then return r(0,as[1],1)
    elseif #as == 2 then return r(as[1],as[2],1)
    else return r(as[1],as[2],as[3])
    end
end

local function cycle(s)
    local s = seq(s)
    if not s then return nil end
    local function go(t)
        return cons(first(t),function()
            if (rest(t)) then return go(rest(t)) else return go(s) end end)
    end
    return go(s)
end

---[[
do
    print("testing collections/seqs")
    local l = lcons(3,lcons(5,lcons(7,nil)))
    assert(is_list(l),"is_list")
    assert(index(2,l) == 7,"index")
    assert(length(l) == 3,"length")
    local m,n = list(3,5,7), list(3,5,7,9)
    assert(equals(l,m),"equals")
    assert(not equals(l,n),"equals")
    assert(equals(m,take(3,n)),"take")
    assert(equals(m,take_while(function(x) return x<8 end,n)),"take_while")
    assert(equals(list(7,9),drop_while(function(x) return x<6 end,n)),"drop_while")
    assert(equals(take(4,replicate("M")),list("M","M","M","M")),"replicate")
    assert(equals(replicate(5,3),repeatedly(5,function() return 3 end)),
        "replicate/repeatedly/optional_times")
    local function add2(x) return 2+x end
    assert(equals(n,take(4,iterate(add2,3))),"iterate")
    assert(equals(list(7,9),drop(2,n)),"drop")
    assert(equals(9,first(drop(3,n))),"drop")
    assert(equals(list(9,7,5,3),reverse(n)),"reverse")
    -- proof of laziness and tail recursion. kind of slow, but works
    -- assert(equals(first(drop(1000000,repeatedly(add2,0))),2*1000000),"tail")
    assert(fold(mult, 1, l) == 105,"fold")
    assert(fold(addi, 0, m) == 15,"fold")
    assert(equals(map(add2,m),
                  filter(function(x) return x ~= 3 end,n)),"map/filter")
    assert(equals(map(add2,m,n),append(map(add2,m),map(add2,n))),"lazy_cat")
    assert(equals(to_list(to_vec(m)), m),"to_list/to_vec")
    local v = vector(2,4,6,8)
    assert(equals(to_vec(to_list(v)), v),"to_list/to_vec")
    assert(equals(to_vec(map(function(x) return x-1 end,n)),v),"map")
    assert(equals(append(take(2,m),v),vector(3,5,2,4,6,8)),"append")
    assert(equals(append(append(l,m),n),concat(l,m,n)),"concat")
    assert(equals(n,flatten(list(list(3),vector(5,7),9))),"flatten")
    assert(to_string(seq("flugelhorn")) == "flugelhorn","to_string")
    assert(equals(map(string.byte,"abc"),vector(97,98,99),"map/seq string"))
    assert(equals(map(string.byte,"def"),range(100,102)),"range")
    assert(equals(take(5,range()),list(0,1,2,3,4)),"range")
    assert(equals(range(30,350,100),vector(30,130,230,330)),"range")
    assert(equals(take(8,(cycle(list(0,1,2)))),list(0,1,2,0,1,2,0,1)),"cycle")
    print("done")
end
--]]

function ifthenelse(pred,th,el)
    return ((pred and (force(th) or true)) or force(el))
end

---[[
do
    print("testing if")
    local x,p = 33
    local t = function(x) return function() return x end end
    assert(ifthenelse(x == 33,1,0) == 1)
    assert(ifthenelse(x == 42,thunk(1),thunk(0)) == 0)
    assert(ifthenelse(x == 42,t(1)) == nil)
    print("done")
end
--]
