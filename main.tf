provider "aws" {
  region = "eu-west-1"
}

resource "aws_ecs_cluster" "herigone-ps-ecs-cluster" {
  name = "herigone-ps-ecs-cluster"
}
