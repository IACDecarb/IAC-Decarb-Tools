# Uses an existing Python runtime; never installs software.
[CmdletBinding()]
param(
    [string]$SourceRef = 'HEAD',
    [string]$TargetPath,
    [string]$Branch,
    [string]$ExpectBase,
    [string]$PythonPath,
    [switch]$Apply
)

$ErrorActionPreference = 'Stop'
$runtimePath = $PythonPath
$runtimePrefix = @()
if (-not $runtimePath) {
    foreach ($candidate in @(
        (Join-Path $PSScriptRoot '.venv\Scripts\python.exe'),
        (Join-Path $env:USERPROFILE '.cache\codex-runtimes\codex-primary-runtime\dependencies\python\python.exe')
    )) {
        if (Test-Path -LiteralPath $candidate -PathType Leaf) {
            $runtimePath = $candidate
            break
        }
    }
}
if (-not $runtimePath) {
    foreach ($name in @('python', 'python3', 'py')) {
        $command = Get-Command $name -CommandType Application -ErrorAction SilentlyContinue | Select-Object -First 1
        if ($command -and $command.Source -notmatch '\\WindowsApps\\') {
            $runtimePath = $command.Source
            if ($name -eq 'py') { $runtimePrefix = @('-3') }
            break
        }
    }
}
if (-not $runtimePath) {
    throw 'No existing Python runtime was found. Supply -PythonPath with your installed python.exe. Nothing was installed.'
}

$migrationArgs = @('-B', (Join-Path $PSScriptRoot 'migrate_fst.py'), '--source-ref', $SourceRef)
if ($TargetPath) { $migrationArgs += @('--target', $TargetPath) }
if ($Branch) { $migrationArgs += @('--branch', $Branch) }
if ($ExpectBase) { $migrationArgs += @('--expect-base', $ExpectBase) }
if ($Apply) { $migrationArgs += '--apply' }
& $runtimePath @runtimePrefix @migrationArgs
if ($LASTEXITCODE -ne 0) { throw "FST migration stopped (exit code $LASTEXITCODE)." }
