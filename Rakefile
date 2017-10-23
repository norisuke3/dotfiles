# -*- Coding: utf-8 -*-

HOME        = ENV['HOME']
PWD         = Dir.pwd
TMP         = "#{PWD}/tmp"
EMACS_D     = "#{HOME}/.emacs.d"
EMACS_LOCAL = "#{HOME}/.emacs.d.local"

dotfiles = %w(
  .zshrc
  .emacs.d
)

desc "init .emacs.d"
task "emacs" do
  # copy backup files for .emacs.d
  sh "ln -sfv #{EMACS_LOCAL}/bookmarks #{EMACS_D}/bookmarks"
end

desc "Make symbolic link"
task "symlink" do
  dotfiles.each do |file|
    sh "ln -sfv #{PWD}/#{file} #{HOME}"
  end
end
