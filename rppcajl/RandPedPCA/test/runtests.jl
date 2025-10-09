using RandPedPCA
using Test
using DelimitedFiles
using MatrixMarket


# @testset "RandPedPCA.jl" begin
#     # Write your tests here.
#     ped = readdlm("ped2.txt", Int)
#     #println(ped)
#     @test_nowarn li = ped2Li(ped)
#     @test size(li) == (size(ped, 1), size(ped, 1))  
# end
# 

@testset "Import sparse matris in MM format" begin
    @test_nowarn li = MatrixMarket.mmread("li2.mtx")
    
    
end