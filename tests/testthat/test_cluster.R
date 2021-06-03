verbose <- 0L
queueName <- "ECSDockerParallelTestQueue"
clusterName <- "docker-parallel-test-cluster"
serverTaskDefName <- "R-server-test-task-definition"
workerTaskDefName <- "R-worker-test-task-definition"
removeAllContainer <- function(clusterName, jobQueueName){
    if(!clusterName%in%ECSListClusters()){
        return()
    }
    workerHandles <- ECSListWorkers(clusterName)
    if(nrow(workerHandles)!=0){
        ECSStopContainers(clusterName = clusterName, ids = workerHandles$id)
    }

    serverHandles <- ECSListServers(clusterName)
    if(nrow(serverHandles)!=0){
        ECSStopContainers(clusterName = clusterName, ids = serverHandles$id)
    }
}

deleteAllDefinition <- function(){
    deleteCluster(clusterName)
    deleteTaskDefinition(serverTaskDefName)
    deleteTaskDefinition(workerTaskDefName)
}

aws.ecx::aws_set_retry_time(20)
aws.ecx::aws_set_network_timeout(5)

test_that("The constructor function", {
    expect_error(provider <- ECSFargateProvider(), NA)
})



if(existCredentials()){
    removeAllContainer(clusterName = clusterName, jobQueueName = queueName)
    provider <- ECSFargateProvider(clusterName = clusterName,
                                   serverTaskDefName = serverTaskDefName,
                                   workerTaskDefName = workerTaskDefName)
    container <-doRedisContainer::doRedisWorkerContainer()

    test_that("general test",{
        # DockerParallel::generalDockerClusterTest(
        #     cloudProvider = provider$copy(),
        #     workerContainer = container$copy(),
        #     workerNumber = 5,
        #     testReconnect = TRUE,
        #     jobQueueName = queueName,
        #     verbose = verbose)
    })

    # We will enable this test after the required packages have been submitted to CRAN
    test_that("The cluster", {
        cluster <- makeDockerCluster(cloudProvider = provider$copy(),
                                     workerContainer = container$copy(),
                                     workerNumber = 1,
                                     jobQueueName = queueName,
                                     verbose = verbose)
        cluster$workerContainer$setRPackages("Jiefei-Wang/ECSFargateProvider")
        expect_error(cluster$startCluster(), NA)

        library(foreach)
        # getDoParWorkers()
        {
            expect_error(
                res <- foreach(i = 1:2) %dopar%{
                    Sys.info()
                }
                ,NA)
        }
        release <- lapply(res, function(x) x[["release"]])

        expect_equal(grep("amzn", release), 1:2)
        expect_error(cluster$stopCluster(), NA)
    })

    test_that("cleanup", {
        workerHandles <- ECSListWorkers(clusterName)
        expect_true(nrow(workerHandles)==0)
        serverHandles <- ECSListServers(clusterName)
        expect_true(nrow(serverHandles)==0)
    })

}
