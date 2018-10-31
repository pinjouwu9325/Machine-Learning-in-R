#Laod data
data <- read.csv("2012MLB.csv", header = T, sep=',')
head(data)

#PCA
pca_data <- prcomp(formula = ~H1B+H2B+H3B+HR+RBI+SB+BB, #select vairable, now is 7
                   data = data, #your data
                   scale = T) #Regularized data���W�����
pca_data
#Standard deviations:�S�x�ȶ}�ڸ�
#Rotation: �S�x�V�q�A�U�ӥD�����ҹ������u�ʲզX(linear combination)�Y��

#How many PCAs do we need?
#Ask Scree plot and Pareto plot for help

##Scree plot
plot(pca_data,  #PCA data here
     type="line",  #create line plot
     main = "Scree Plot for 2012MLB")  #main title

#red line indicate the vairance�S�x��=1 
#Kaiser eigenvalue-greater-than-one rule,
#depends on this rule, we could pick the PCA which is greater than one 
abline(h=1, col="red")
#here, we select the first to the third PCA

##Pareto plot
#�p��S�x��variance(=srd^2)
vars <- (pca_data$sdev)^2
vars

#�p��C�ӥD�������������(�U�ӥD�������S�x��variance/�`�S�x��total variance) 
props <- vars/sum(vars)
props

#aggregated effects�֥[�C�ӥD������������� 
#cumcum:Cumulative Sums
cumlative_props <- cumsum(props)
cumlative_props

#plot the aggregated effects
plot(cumlative_props)
cumlative_props[3] #when we select the first to the third PC, it could explain 70.64% variance

#After PCA, the original data will be transfer to the new dataset replaced by PC
#select the first to the third PC and replace them in to the original data, making a new dataset
#pca$rotation
top3_pca_data <- pca_data$x[, 1:3] #PC1-PC3
top3_pca_data


##�D�����t��PC loading:�D����PC�M���ܼƪ����Y
#Each PC���O���ܼƸg�L�u�ʲզX�Უ�ͪ���
#�n����PC�G�[��PC�M���ܼƶ������Y�A�]�N�O�[����ܼƦb�u�ʲզX�����Y��(�S�x�V�q)
#�ݭ��ܼƹ�PC�s���O�����٬O�t���B���h�j���v�T

#�S�x�V�q(���ܼƪ��u�ʲզX)
pca_data$rotation

#Select the first to the third
top3_pca_eigenvector <- pca_data$rotation[, 1:3]
top3_pca_eigenvector

#plot loading plot for PCs �[����ܼƻPPC���������Y
first_pca <- top3_pca_eigenvector[, 1]
second_pca <- top3_pca_eigenvector[, 2]
third_pca <- top3_pca_eigenvector[, 3]

#PC1
#SB(�s�S)�BBB(�|�a)�PPC1�e�������A�ݰ_�ӬݩM"�W�S"����
first_pca[order(first_pca, decreasing = F)]
dotchart(first_pca[order(first_pca, decreasing = F)], 
         main = "Loading Plot for PC1",
         xlab = "Variable Laodings",
         color = "red")

#PC2
#HR(homerun)�BBB�BRBI(���I)�PPC2�e�������A�ݰ_�өM"������"����
second_pca[order(second_pca, decreasing = F)]
dotchart(second_pca[order(second_pca, decreasing = F)],
         main = "Loading Plot for PC2",
         xlab = "Vairable Loadings",
         color = "blue")

#PC3
#H1B(�@�S�w��)�BH2B(�G�S�w��)�PPC3�e�������A�ݰ_�өM"�w��"����
third_pca[order(third_pca, decreasing = F)]
dotchart(third_pca[order(third_pca, decreasing = F)],
         main = "Loading Plot for PC3",
         xlab = "Variable Loadings",
         color = "dark green")

#�t�@��PC���R�ϡA�[��C�Ӳy��(obs.)���ժ��S�ʬO����
#Select PC1, PC2 to plot a loading plot
biplot(pca_data, choices=1:2)

#�k�䪺�y���A�X�W�S�A�h�H�s�S(SB)�M�|�a�y(BB)�O�e�����A���S�����{����(e.g.�s��19)
#���W�誺�y���H�O�q���ӡA�b���S��(HR)�M���I(RBI)�W����۪��u��(e.g.�s��11)
#�U�誺�y�����ժ����S���A���b�w���W�����{���ө��L�y���A�s�S�]���@�w����(e.g.�s��5)