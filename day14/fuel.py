import numpy as np


with open("input.txt") as file:
    lines = file.read().splitlines()

from_tos = [tuple(part.split(', ') for part in line.split(' => '))
          for line in lines]

def read(s):
    [i,t] = s.split(' ')
    return (t,int(i))

batchsize_map = {'ORE': 1}
recipe_map = {'ORE': {'ORE':1}}
for (reqs, [res]) in from_tos:
    (t,i) = read(res)
    batchsize_map[t] = i
    recipe_map[t] = dict(read(req) for req in reqs)

# ^ IO v NP

types = list(recipe_map.keys())

batchsize = np.array([[batchsize_map[t]] for t in types], dtype=int)
transmission = np.array([[recipe_map[colt].get(rowt,0) for colt in types] for rowt in types], dtype=int)

idfor = lambda t: np.array([[1 if ot == t else 0] for ot in types], dtype=int)

need = idfor("FUEL")
have = np.zeros(need.shape, dtype=int)
while True:
    need_met = np.min(np.concatenate([need, have], axis=1), axis=1, keepdims=True)
    have -= need_met
    need -= need_met
    
    batched_need = (need + batchsize - 1) // batchsize
    new_need = np.dot(transmission, batched_need)
    new_have = have + batched_need * batchsize - need

    (need, have) = (new_need, new_have)
    
    if (np.sum(idfor("ORE") * need) == np.sum(need)):
        break

print (np.sum(need))
