#relacão em um conjunto

class Relacao:
    def __init__(self, R, setR):
        self.R = R
        self.n = len(R)
        self.setR = setR

        matrix = []
        for i in range(len(self.setR)):
            matrix.append([0]*len(self.setR))
        self.matrix = matrix
        for i in self.R:
            matrix[i[0]-1][i[1]-1] = 1

        

    def is_reflexive(self):
        status = []
        for i in self.setR:
            status.append((i,i) in self.R)
        if(all(status)):
            return True
        else:
            return False

    def is_simetric(self):
        status = []
        for i in self.R:
            revi = (i[1], i[0])
            status.append( (i in self.R) and (revi in self.R))
        if(all(status)):
            return True
        else:
            return False

    def is_irreflexive(self):
        status = []
        for i in self.setR:
            status.append((i,i) not in self.R)
        if(all(status)):
            return True
        else:
            return False

    def is_antisimetric(self):
        status = []
        for i in self.R:
            revi = (i[1], i[0])
            if(revi == i):
                status.append(True)
            else:
                status.append(not((i in self.R) and (revi in self.R)))

        if(all(status)):
            return True
        else:
            return False
