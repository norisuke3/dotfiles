# -*- Coding: utf-8 -*-

HOME        = ENV['HOME']
PWD         = Dir.pwd
TMP         = "#{PWD}/tmp"
EMACS_D     = "#{HOME}/.emacs.d"
EMACS_LOCAL = "#{HOME}/.emacs.d.local"
SITE_LISP   = "#{EMACS_D}/site-lisp"

# wget で直接ダウンロードするパッケージ
packages = %w(
  http://www.gentei.org/~yuuji/software/euc/windows.el
  http://www.gentei.org/~yuuji/software/euc/revive.el
  http://mibai.tec.u-ryukyu.ac.jp/~oshiro/Programs/elisp/kill-summary.el
)

dotfiles = %w(
  .zshrc
)

namespace :emacs do
  desc "initialize .emacs.d"
  task :init => :clean do
    sh "ln -svfn #{PWD}/.emacs.d #{HOME}/.emacs.d"
    sh "ln -sfv #{EMACS_LOCAL}/bookmarks #{EMACS_D}/bookmarks"
    sh "ln -sfv #{EMACS_LOCAL}/80-local.el #{EMACS_D}/inits/80-local.el"

    Rake::Task["emacs:init-loader"].execute
    Rake::Task["emacs:download-packages"].execute
  end

  desc "clean emacs.d"
  task :clean do
    sh "rm -rf #{EMACS_D}"
  end

  desc "list emacs files"
  task :ls do
    sh "ls -la #{EMACS_D}"
    sh "ls -la #{EMACS_D}/"
  end

  desc "downloads init-loader"
  task :"init-loader" do
    sh "rm -rf #{TMP}/init-loader"
    sh "git clone https://github.com/emacs-jp/init-loader.git #{TMP}/init-loader"
    sh "cp #{TMP}/init-loader/init-loader.el #{SITE_LISP}/init-loader.el"
  end

  desc "download packages which is not in the package repository"
  task "download-packages" do
    packages.each do |p|
      file = p.split('/').last
      sh "cd #{TMP}; wget #{p}"
      sh "cp #{TMP}/#{file} #{SITE_LISP}/#{file}"
    end
  end
end

desc "Make symbolic link"
task "symlink" do
  dotfiles.each do |file|
    sh "ln -sfv #{PWD}/#{file} #{HOME}"
  end
end
