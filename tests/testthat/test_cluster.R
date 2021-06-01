verbose <- 0L
queueName <- "ECSDockerParallelTestQueue"
clusterName <- "docker-parallel-test-cluster"
serverTaskDefName <- "R-server-test-task-definition"
workerTaskDefName <- "R-worker-test-task-definition"
removeAllContainer <- function(clusterName, jobQueueName){
    if(!clusterName%in%listClusters()){
        return()
    }
    handles <- listTasks(clusterName)
    runningWorkers <- NULL
    if(!is.null(handles)){
        info <- ecs_describe_tasks(cluster = clusterName, tasks = handles)
        for(i in info$tasks){
            env <- i$overrides$containerOverrides[[1]]$environment
            env <- ArrayToList(env)
            if(identical(env[["ECSFargateClusterJobQueueName"]], jobQueueName)){
                runningWorkers <- c(runningWorkers, i$taskArn)
                message(i$taskArn)
            }
        }
    }
    if(!is.null(runningWorkers)){
        stopTasks(clusterName = clusterName, taskIds = runningWorkers)
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
        DockerParallel::generalDockerClusterTest(
            cloudProvider = provider,
            workerContainer = container,
            workerNumber = 5,
            testReconnect = TRUE,
            jobQueueName = queueName,
            verbose = verbose)
    })

    # We will enable this test after the required packages have been submitted to CRAN
    test_that("The cluster", {
        cluster <- makeDockerCluster(cloudProvider = provider,
                                     workerContainer = container,
                                     workerNumber = 1,
                                     jobQueueName = queueName,
                                     verbose = verbose)

        expect_error(cluster$startCluster(), NA)

        library(foreach)
        # getDoParWorkers()
        expect_error(
            res <- foreach(i = 1:2) %dopar%{
                Sys.info()
            }
            ,NA)
        release <- lapply(res, function(x) x[["release"]])

        expect_equal(grep("amzn", release), 1:2)
        expect_error(cluster$stopCluster(), NA)
    })

}
