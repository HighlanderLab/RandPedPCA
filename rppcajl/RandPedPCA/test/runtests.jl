using RandPedPCA
using Test
using DelimitedFiles
using MatrixMarket
using LinearAlgebra

# @testset "RandPedPCA.jl" begin
#     # Write your tests here.
#     ped = readdlm("ped2.txt", Int)
#     #println(ped)
#     @test_nowarn li = ped2Li(ped)
#     @test size(li) == (size(ped, 1), size(ped, 1))  
# end
# 

@testset "Import sparse matrix in MM format" begin
    rr = randn(2650)
    @test_nowarn li = MatrixMarket.mmread("li2.mtx")
    li = MatrixMarket.mmread("li2.mtx")
    ai = li' * li
    @test all(li \ (li' \ rr) .≈ ai \ rr)
    
end

@testset "Range finder" begin
    li = MatrixMarket.mmread("li2.mtx")
    Q = rangeFinder(li, 10, 2, 20)
    
    @test size(Q) == (size(li, 1), 10)
    @test_nowarn rSVD(li, 10, 2, 20)

end



