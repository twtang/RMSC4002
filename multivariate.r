Attribute VB_Name = "Multivariate"
Option Explicit
Option Base 1


Function mcov(data As Range) As Variant

  Dim i As Integer
  Dim j As Integer
  Dim nc As Integer
  nc = data.Columns.Count
  Dim cov() As Double
  ReDim cov(nc, nc)
  
  For i = 1 To nc
    For j = 1 To nc
      cov(i, j) = Application.WorksheetFunction.Covar(data.Columns(i), data.Columns(j))
    Next j
  Next i
  mcov = cov
  
End Function

Function mcor(data As Range) As Variant

  Dim i As Integer
  Dim j As Integer
  Dim nc As Integer
  nc = data.Columns.Count
  Dim cov() As Double
  ReDim cov(nc, nc)
  
  For i = 1 To nc
    For j = 1 To nc
      cov(i, j) = Application.WorksheetFunction.Correl(data.Columns(i), data.Columns(j))
    Next j
  Next i
  mcor = cov
  
End Function

Function cholesky(A As Range) As Variant

  Dim i, j, k As Integer
  Dim sum As Double
  Dim p As Integer
  p = A.Columns.Count
  Dim U() As Double
  ReDim U(p, p)
  
  U(1, 1) = Sqr(A(1, 1))
  
  For j = 2 To p
    U(1, j) = A(1, j) / U(1, 1)
  Next j
  
  For i = 2 To p
    sum = 0
    For k = 1 To i - 1
      sum = sum + U(k, i) * U(k, i)
    Next k
    U(i, i) = Sqr(A(i, i) - sum)
    
    For j = i + 1 To p
      sum = 0
      For k = 1 To i - 1
        sum = sum + U(k, i) * U(k, j)
      Next k
      U(i, j) = (A(i, j) - sum) / U(i, i)
      U(j, i) = 0
    Next j
  Next i
  
  cholesky = U
    
End Function



