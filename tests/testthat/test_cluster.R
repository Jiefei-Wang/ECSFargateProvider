context("Testing the provider")

if(existCredentials()){
    aws.ecx::aws_set_retry_time(20)
    aws.ecx::aws_set_network_timeout(5)
    workerPerContainer <- 2L
    provider <- ECSFargateProvider()
    container <- BiocFEDRContainer::BiocFEDRWorkerContainer()
    container$maxWorkerNum <- workerPerContainer
    generalDockerClusterTest(cloudProvider = provider, workerContainer = container)
}
