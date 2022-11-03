# Essex.r
# based on RScs1801_3rd.r
# Written by Eugene.Gallagher@umb.edu 10/30/22
# it's easy to enter the data from Phibrick's 'In the Heart of the Sea'
myTable <- matrix(c(5,1,4,7), nrow = 2, ncol = 2, byrow = TRUE,
                  dimnames = list(c("Black", "White"),c("Died", "Survived")))
myTable   # Show the table

## INFERENCE (4 methods for getting p-values and confidence intervals)
prop.test(myTable, alternative="greater", correct=FALSE) # Compare 2 proportions
prop.test(myTable, alternative="greater", correct=TRUE) # ...with Yates continuity correction
prop.test(myTable,correct=TRUE) # 2-sided alternative (default) to get CI
chisq.test(myTable) # Pearson's Chi-Squared Test
fisher.test(myTable, alternative="greater") # Fisher's exact test
fisher.test(myTable) # 2-sided alternative to get CI for odds ratio
