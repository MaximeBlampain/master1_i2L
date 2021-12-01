{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module GithubApi where

import Data.Text
import Servant

import Repo
import User

type AgentHeader = Header "User-Agent" Text

-- api.github.com/users/<user>
type UserApi = "users" :> Capture "user" Text :> AgentHeader :> Get '[JSON] (Maybe User)

-- api.github.com/repositories/<repo>
type RepoApi = "repositories" :> Capture "repo" Int :> AgentHeader :> Get '[JSON] (Maybe Repo) 

-- api.github.com/users/<user>/repos
type UserReposApi = "users" :> Capture "user" Text :> "repos" :> AgentHeader :> Get '[JSON] [Repo]

