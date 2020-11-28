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
  https://www.emacswiki.org/emacs/download/yasnippet-config.el
  https://www.emacswiki.org/emacs/download/point-undo.el
)

dotfiles = %w(
  .zshrc
  .screenrc
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

  namespace :packages do
    desc "take a snap shot of the current packages."
    task :snapshot do
      ts = Time.now.strftime("%Y_%m_%d_%H%M%S")
      sh "cd #{EMACS_D}; tar -zcvf #{EMACS_LOCAL}/elpa_#{ts}.tar.gz elpa"
    end
  end
end

desc "Make symbolic link"
task "symlink" do
  dotfiles.each do |file|
    sh "ln -sfv #{PWD}/#{file} #{HOME}"
  end
end
