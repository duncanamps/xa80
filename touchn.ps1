#
# Touch all the files and folders recursively to set the date
# and time to now. But only if the files have changed in the last
# Age days. Call with ./tchn <days>
#

Param($Age) 
(Get-ChildItem -Path . -Recurse -Exclude ".git" ) | Where-Object { ($_ -notlike "*.git*") -and ($_.LastWriteTime -gt (Get-Date).AddDays(-$Age)) } | ForEach-Object {$_.LastWriteTime = (Get-Date)}
