module RandPedPCA


# TODO:
# - centring

# Write your package code here.
using LinearAlgebra
using DelimitedFiles

export ped2Li, rangeFinder, rSVD, rppca

function ocarleLi(li, x)
    return(li \ (li' \ x))
end # function ocarleLi

function rangeFinder(li, rank, depth, numVectors)
    n = size(li, 1)
    Ω = randn(n, numVectors)
    Y = li * Ω
    
    Q = Matrix(qr(Y).Q)
    for i in 2:depth
        Y = ocarleLi(li, Q)
        Q = Matrix(qr(Y).Q)
    end

    return Q[:, 1:rank]
end # function rangeFinder



function rSVD(li, rank, depth, numVectors)
    Q = rangeFinder(li, rank, depth, numVectors)
    B = (li' \ Q)'
    U, S, V = svd(B)
    UU = Q * U
    return UU[:,1:rank], S[1:rank], V[:,1:rank]
end # function rSVD


function rppca(x, rank, depth, numVectors)
    U,S,_ = rSVD(x, rank, depth, numVectors)
    return U * diagm(S.^2), S
end # function rppca

end # module