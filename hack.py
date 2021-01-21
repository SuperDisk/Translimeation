def load_table(fname):
    with open(fname,'r', encoding='utf-8') as f:
        lines = list(f)
        hex2j = {}
        j2hex = {}
        for line in lines:
            l,r = line.split('=')
            hex2j[l]=r.strip()
            j2hex[r.strip()]=l

        return hex2j, j2hex
    
def hexify(j, j2hex):
    out = ""
    for letter in j:
        out += j2hex[letter]
    return out
