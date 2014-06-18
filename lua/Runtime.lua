-- mooninite Lisp-1 -> Lua runtime
--
-- require "Runtime" to access. Should be done automatically when using
-- mooninite.

-- imports from global libs
--

-- don't pollute global space inadvertantly
--_ENV = nil

-- exports at end.

local function id (...) return ... end

local function thunk(value)
    return {value}
end

local function force(thunk)
    if thunk[2] then return thunk[2] end
    local t = thunk
    while type(t) == 'table' do
        t = t[1]
    end
    if type(thunk) == 'table' then
        if type(t) == 'function' then
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

--   Lists.

local function cons(car,cdr)
    return {car, cdr; cons = true}
end

local function car(l) return l[1] end

local function cdr(l) return l[2] end

local function is_cons(x) return type(x) == 'table' and x['cons'] end

local function list(...)
    local xs,out = {...},nil
    for i = #xs, 1, -1 do
        out = cons(xs[i],out)
    end
    return out;
end

local function index(ind,l)
    local l = l
    for i = 1, ind do
        l = cdr(l)
    end
    return car(l)
end

local function length(list,max)
    if (not list) then return 0 end
    local l,n = list, 1
    while cdr(l) do
        n = n + 1
        l = cdr(l)
    end
    return n
end

local function fold(f,init,xs)
    if car(xs) == nil then return init end
    repeat
        init = f(init,car(xs))
        xs = cdr(xs)
    until not xs
    return init
end

local function equals(l,r)
    if l == r then return true
    elseif (is_cons(l) and is_cons(r)) then
        return equals(car(l),car(r)) and equals(cdr(l),cdr(r))
    else return false
    end
end

local function filter(f,xs)
    if not xs then return nil
    elseif f(car(xs)) then return cons(car(xs),filter(f,cdr(xs)))
    else return filter(f,cdr(xs))
    end
end

local function map(f,xs)
    if not xs then return nil
    else return cons(f(car(xs)),map(f,cdr(xs)))
    end
end

local function append(xs,ys)
    if not xs then return ys
    else return cons(car(xs),append(cdr(xs),ys))
    end
end

---[[
    assert(length(nil) == 0)
    assert(not is_cons(nil))
    assert(not is_cons({234}))
    assert(length(cons(44)) == 1)
    local l = cons(3,cons(5,cons(7,nil)))
    assert(is_cons(l))
    assert(index(2,l) == 7)
    assert(length(l) == 3)
    local m,n = list(3,5,7), list(3,5,7,9)
    assert(equals(l,m))
    assert(not equals(l,n))
    assert(fold(function(x,y) return x * y end, 1, l) == 105)
    assert(fold(function(x,y) return x + y end, 0, m) == 15)
    assert(equals(map(function(x) return x + 2 end,m),
                  filter(function(x) return x ~= 3 end,n)))
    assert(length(append(l,m)) == 6)
--]]
