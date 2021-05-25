


test_that("The constructor function", {
    expect_error(provider <- ECSFargateProvider(), NA)
})

# We will enable this test after the required packages have been submitted to CRAN
# test_that("The cluster", {
#     if(existCredentials()){
#         aws.ecx::aws_set_retry_time(20)
#         aws.ecx::aws_set_network_timeout(5)
#         provider <- ECSFargateProvider()
#         container <-doRedisContainer::doRedisWorkerContainer()
#         cluster <- makeDockerCluster(cloudProvider = provider,
#                                      workerContainer = container,
#                                      workerNumber = 1)
#         cluster$startCluster()
#
#         library(foreach)
#         getDoParWorkers()
#         foreach(i = 1:2) %dopar%{
#             Sys.info()
#         }
#
#         cluster$stopCluster()
#     }
# })

