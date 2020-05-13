import de.heikoseeberger.sbtfresh.license.License // Only needed for `freshLicense` setting

freshOrganization     := "Pocket Plane Inc"        // Organization – "default" by default
freshAuthor           := "Adam James McCullough"        // Author – value of "user.name" system property or "default" by default
freshLicense          := Some(License.mit) // Optional license – `apache20` by default
freshSetUpGit         := true              // Initialize a Git repo and create an initial commit – `true` by default
freshSetUpTravis      := true              // Configure Travis for Continuous Integration - `false` by default
freshSetUpWartremover := true              // Include the sbt wartremover (http://www.wartremover.org) plugin - `false` by default
