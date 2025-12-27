%% =============================================================================================
%                                        P R O J E C T
%                                              of
%    Climate hazards and mental health hospitalizations in China: socioeconomic disparities, 
%                           demographic drivers, and adaptation
%
%% =============================================================================================

%  Developed by Teng Wang @ HKU
%    Contact: wang.teng19@alumni.imperial.ac.uk
%
%  Descriptions: Errorbar plot
%    This is designed to for post-processing visualization


%% Hospitalizations

% Overall
R_Plot_ErrorBar(1,[2.03,0,0],-2,'#de2c35',0.5,1)  % Aging % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(1,[0.89,0,0],-4,'#de2c35',0.5,0.5)  % Grow % FigNo,DataSrc,yloc,MarkerColor,width,alpha

% Storm
R_Plot_ErrorBar(1,[1.03,0,0],-8,'#fa6502',0.5,1)  % Aging % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(1,[0.17,0,0],-10,'#fa6502',0.5,0.5)  % Grow % FigNo,DataSrc,yloc,MarkerColor,width,alpha

% Flood
R_Plot_ErrorBar(1,[2.11,0,0],-14,'#f2b90f',0.5,1)  % Aging % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(1,[0.69,0,0],-16,'#f2b90f',0.5,0.5)  % Grow % FigNo,DataSrc,yloc,MarkerColor,width,alpha

% Cyclone
R_Plot_ErrorBar(1,[3.05,0,0],-20,'#508896',0.5,1)  % Aging % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(1,[2.76,0,0],-22,'#508896',0.5,0.5)  % Grow % FigNo,DataSrc,yloc,MarkerColor,width,alpha

figure(1)

xlim([0 5])
ylim([-24 0])
xlabel('Hospitalization contribution (%)');

yticks(flip(-[2,4,8,10,14,16,20,22]));

yticklabels({'Growth', 'Aging', 'Growth', 'Aging', 'Growth', 'Aging', 'Growth', 'Aging'});% 倒序



%% DALYs

% Overall
R_Plot_ErrorBar(2,[2.84,0,0],-2,'#de2c35',0.5,1)  % Aging % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(2,[0.80,0,0],-4,'#de2c35',0.5,0.5)  % Grow % FigNo,DataSrc,yloc,MarkerColor,width,alpha

% Storm
R_Plot_ErrorBar(2,[1.41,0,0],-8,'#fa6502',0.5,1)  % Aging % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(2,[0.14,0,0],-10,'#fa6502',0.5,0.5)  % Grow % FigNo,DataSrc,yloc,MarkerColor,width,alpha

% Flood
R_Plot_ErrorBar(2,[2.93,0,0],-14,'#f2b90f',0.5,1)  % Aging % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(2,[0.62,0,0],-16,'#f2b90f',0.5,0.5)  % Grow % FigNo,DataSrc,yloc,MarkerColor,width,alpha

% Cyclone
R_Plot_ErrorBar(2,[4.49,0,0],-20,'#508896',0.5,1)  % Aging % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(2,[2.62,0,0],-22,'#508896',0.5,0.5)  % Grow % FigNo,DataSrc,yloc,MarkerColor,width,alpha

figure(2)

ylim([-24 0])
xlim([0 5])
xlabel('DALYs contribution (%)');

yticks(flip(-[2,4,8,10,14,16,20,22]));

yticklabels({'Growth', 'Aging', 'Growth', 'Aging', 'Growth', 'Aging', 'Growth', 'Aging'});% 倒序



%% Hosp per 100k population

% Storm
R_Plot_ErrorBar(100,[4.57,3.85,5.56],-2,'#135ae1',0.5,0.6)  % Aging % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(100,[4.16,3.20,5.31],-4,'#2197ff',0.5,0.6)  % Grow % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(100,[9.86,7.80,12.42],-6,'#9852d9',0.5,0.6)  % Aging % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(100,[19.41,15.47,23.93],-8,'#f54e8b',0.5,0.6)  % Grow % FigNo,DataSrc,yloc,MarkerColor,width,alpha

figure(100)

ylim([-9 -1])
xlim([0 25])
xlabel('Storm-attributable hospitalizations per 100,000 population');

yticks(flip(-[2,4,6,8]));

yticklabels({'Age ≥ 65', 'Age 45-64', 'Age 20-44', 'Age ≤19'});% 倒序



% Flood
R_Plot_ErrorBar(101,[6.19,5.22,7.52],-2,'#135ae1',0.5,0.6)  % Aging % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(101,[5.73,4.41,7.32],-4,'#2197ff',0.5,0.6)  % Grow % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(101,[13.72,10.86,17.29],-6,'#9852d9',0.5,0.6)  % Aging % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(101,[23.30,18.57,28.73],-8,'#f54e8b',0.5,0.6)  % Grow % FigNo,DataSrc,yloc,MarkerColor,width,alpha

figure(101)

ylim([-9 -1])
xlim([0 30])
xlabel('Flood-attributable hospitalizations per 100,000 population');

yticks(flip(-[2,4,6,8]));
yticklabels({'Age ≥ 65', 'Age 45-64', 'Age 20-44', 'Age ≤19'});% 倒序



% Cyclone
R_Plot_ErrorBar(102,[2.99,2.52,3.63],-2,'#135ae1',0.5,0.6)  % Aging % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(102,[4.03,3.10,5.15],-4,'#2197ff',0.5,0.6)  % Grow % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(102,[9.40,7.44,11.85],-6,'#9852d9',0.5,0.6)  % Aging % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(102,[15.28,12.18,18.84],-8,'#f54e8b',0.5,0.6)  % Grow % FigNo,DataSrc,yloc,MarkerColor,width,alpha

figure(102)

ylim([-9 -1])
xlim([0 30])
xlabel('Flood-attributable hospitalizations per 100,000 population');

yticks(flip(-[2,4,6,8]));
yticklabels({'Age ≥ 65', 'Age 45-64', 'Age 20-44', 'Age ≤19'});% 倒序



%% Hosp Age

% Storm
R_Plot_ErrorBar(1000,[4.57,3.85,5.56],-2,'#fa6502',0.5,1)  % Aging % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(1000,[4.16,3.20,5.31],-4,'#fa6502',0.5,0.8)  % Grow % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(1000,[9.86,7.80,12.42],-6,'#fa6502',0.5,0.6)  % Aging % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(1000,[19.41,15.47,23.93],-8,'#fa6502',0.5,0.4)  % Grow % FigNo,DataSrc,yloc,MarkerColor,width,alpha

% Flood
R_Plot_ErrorBar(1000,[6.19,5.22,7.52],-12,'#f2b90f',0.5,1)  % Aging % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(1000,[5.73,4.41,7.32],-14,'#f2b90f',0.5,0.8)  % Grow % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(1000,[13.72,10.86,17.29],-16,'#f2b90f',0.5,0.6)  % Aging % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(1000,[23.30,18.57,28.73],-18,'#f2b90f',0.5,0.4)  % Grow % FigNo,DataSrc,yloc,MarkerColor,width,alpha

% Cyclone
R_Plot_ErrorBar(1000,[2.99,2.52,3.63],-22,'#508896',0.5,1)  % Aging % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(1000,[4.03,3.10,5.15],-24,'#508896',0.5,0.8)  % Grow % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(1000,[9.40,7.44,11.85],-26,'#508896',0.5,0.6)  % Aging % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(1000,[15.28,12.18,18.84],-28,'#508896',0.5,0.4)  % Grow % FigNo,DataSrc,yloc,MarkerColor,width,alpha

Plot_ErrorBar(1000,[4.57,3.85,5.56],-2)
Plot_ErrorBar(1000,[4.16,3.20,5.31],-4)
Plot_ErrorBar(1000,[9.86,7.80,12.42],-6)
Plot_ErrorBar(1000,[19.41,15.47,23.93],-8)

Plot_ErrorBar(1000,[6.19,5.22,7.52],-12)
Plot_ErrorBar(1000,[5.73,4.41,7.32],-14)
Plot_ErrorBar(1000,[13.72,10.86,17.29],-16)
Plot_ErrorBar(1000,[23.30,18.57,28.73],-18)

Plot_ErrorBar(1000,[2.99,2.52,3.63],-22)
Plot_ErrorBar(1000,[4.03,3.10,5.15],-24)
Plot_ErrorBar(1000,[9.40,7.44,11.85],-26)
Plot_ErrorBar(1000,[15.28,12.18,18.84],-28)

figure(1000)

ylim([-30 0])
xlim([0 30])
xlabel('Hazard-attributable hospitalizations per 100,000 population');

yticks(flip(-[2,4,6,8,12,14,16,18,22,24,26,28]));

yticklabels({'Age ≥ 65', 'Age 45-64', 'Age 20-44', 'Age ≤19', ...
    'Age ≥ 65', 'Age 45-64', 'Age 20-44', 'Age ≤19', ...
    'Age ≥ 65', 'Age 45-64', 'Age 20-44', 'Age ≤19'});% 倒序



%% DALYs Age

% Storm
R_Plot_ErrorBar(1001,[0.0386,0.0233,0.0616],-2,'#fa6502',0.5,1)  % Aging % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(1001,[0.0143,0.0082,0.0234],-4,'#fa6502',0.5,0.8)  % Grow % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(1001,[0.0479,0.0283,0.0774],-6,'#fa6502',0.5,0.6)  % Aging % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(1001,[0.1567,0.0918,0.2497],-8,'#fa6502',0.5,0.4)  % Grow % FigNo,DataSrc,yloc,MarkerColor,width,alpha

% Flood
R_Plot_ErrorBar(1001,[0.0523,0.0315,0.0834],-12,'#f2b90f',0.5,1)  % Aging % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(1001,[0.0197,0.0113,0.0323],-14,'#f2b90f',0.5,0.8)  % Grow % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(1001,[0.0667,0.0394,0.1077],-16,'#f2b90f',0.5,0.6)  % Aging % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(1001,[0.1882,0.1103,0.2998],-18,'#f2b90f',0.5,0.4)  % Grow % FigNo,DataSrc,yloc,MarkerColor,width,alpha

% Cyclone
R_Plot_ErrorBar(1001,[0.0252,0.0152,0.0403],-22,'#508896',0.5,1)  % Aging % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(1001,[0.0139,0.0079,0.0227],-24,'#508896',0.5,0.8)  % Grow % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(1001,[0.0457,0.0270,0.0738],-26,'#508896',0.5,0.6)  % Aging % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(1001,[0.1234,0.0723,0.1966],-28,'#508896',0.5,0.4)  % Grow % FigNo,DataSrc,yloc,MarkerColor,width,alpha


Plot_ErrorBar(1001,[0.0386,0.0233,0.0616],-2)
Plot_ErrorBar(1001,[0.0143,0.0082,0.0234],-4)
Plot_ErrorBar(1001,[0.0479,0.0283,0.0774],-6)
Plot_ErrorBar(1001,[0.1567,0.0918,0.2497],-8)

Plot_ErrorBar(1001,[0.0523,0.0315,0.0834],-12)
Plot_ErrorBar(1001,[0.0197,0.0113,0.0323],-14)
Plot_ErrorBar(1001,[0.0667,0.0394,0.1077],-16)
Plot_ErrorBar(1001,[0.1882,0.1103,0.2998],-18)

Plot_ErrorBar(1001,[0.0252,0.0152,0.0403],-22)
Plot_ErrorBar(1001,[0.0139,0.0079,0.0227],-24)
Plot_ErrorBar(1001,[0.0457,0.0270,0.0738],-26)
Plot_ErrorBar(1001,[0.1234,0.0723,0.1966],-28)

figure(1001)

ylim([-30 0])
xlim([0 0.3])
xlabel('Hazard-attributable health loss per 100,000 population (DALYs)');


yticks(flip(-[2,4,6,8,12,14,16,18,22,24,26,28]));

yticklabels({'Age ≥ 65', 'Age 45-64', 'Age 20-44', 'Age ≤19', ...
    'Age ≥ 65', 'Age 45-64', 'Age 20-44', 'Age ≤19', ...
    'Age ≥ 65', 'Age 45-64', 'Age 20-44', 'Age ≤19'});




%% ==== 2nd version plot ==================== Age Hazard Structure ===========================


%% Hosp Age

% Age ≤ 19
R_Plot_ErrorBar(2000,[4.57,3.85,5.56],-2,'#135ae1',0.5,1)  % Aging % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(2000,[6.19,5.22,7.52],-4,'#135ae1',0.5,0.7)  % Aging % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(2000,[2.99,2.52,3.63],-6,'#135ae1',0.5,0.4)  % Aging % FigNo,DataSrc,yloc,MarkerColor,width,alpha

% Age 20-44
R_Plot_ErrorBar(2000,[4.16,3.20,5.31],-10,'#2197ff',0.5,1)  % Grow % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(2000,[5.73,4.41,7.32],-12,'#2197ff',0.5,0.7)  % Grow % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(2000,[4.03,3.10,5.15],-14,'#2197ff',0.5,0.4)  % Grow % FigNo,DataSrc,yloc,MarkerColor,width,alpha

% Age 45-64
R_Plot_ErrorBar(2000,[9.86,7.80,12.42],-18,'#9852d9',0.5,1)  % Aging % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(2000,[13.72,10.86,17.29],-20,'#9852d9',0.5,0.7)  % Aging % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(2000,[9.40,7.44,11.85],-22,'#9852d9',0.5,0.4)  % Aging % FigNo,DataSrc,yloc,MarkerColor,width,alpha

% Age 65
R_Plot_ErrorBar(2000,[19.41,15.47,23.93],-26,'#f54e8b',0.5,1)  % Grow % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(2000,[23.30,18.57,28.73],-28,'#f54e8b',0.5,0.7)  % Grow % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(2000,[15.28,12.18,18.84],-30,'#f54e8b',0.5,0.4)  % Grow % FigNo,DataSrc,yloc,MarkerColor,width,alpha


% 95CI
Plot_ErrorBar(2000,[4.57,3.85,5.56],-2)
Plot_ErrorBar(2000,[6.19,5.22,7.52],-4)
Plot_ErrorBar(2000,[2.99,2.52,3.63],-6)

Plot_ErrorBar(2000,[4.16,3.20,5.31],-10)
Plot_ErrorBar(2000,[5.73,4.41,7.32],-12)
Plot_ErrorBar(2000,[4.03,3.10,5.15],-14)

Plot_ErrorBar(2000,[9.86,7.80,12.42],-18)
Plot_ErrorBar(2000,[13.72,10.86,17.29],-20)
Plot_ErrorBar(2000,[9.40,7.44,11.85],-22)

Plot_ErrorBar(2000,[19.41,15.47,23.93],-26)
Plot_ErrorBar(2000,[23.30,18.57,28.73],-28)
Plot_ErrorBar(2000,[15.28,12.18,18.84],-30)

figure(2000)

ylim([-32 0])
xlim([0 30])
xlabel('Hazard-attributable hospitalizations per 100,000 population');


yticks(flip(-[2,4,6,10,12,14,18,20,22,26,28,30]));

yticklabels({'TCs', 'Floods', 'Storms', ...
    'TCs', 'Floods', 'Storms', ...
    'TCs', 'Floods', 'Storms', ...
    'TCs', 'Floods', 'Storms'});



%% DALYs Age

% Age ≤19
R_Plot_ErrorBar(2001,[0.0386,0.0233,0.0616],-2,'#135ae1',0.5,1)  % Aging % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(2001,[0.0523,0.0315,0.0834],-4,'#135ae1',0.5,0.7)  % Aging % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(2001,[0.0252,0.0152,0.0403],-6,'#135ae1',0.5,0.4)  % Aging % FigNo,DataSrc,yloc,MarkerColor,width,alpha

% Age 20-44
R_Plot_ErrorBar(2001,[0.0143,0.0082,0.0234],-10,'#2197ff',0.5,1)  % Grow % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(2001,[0.0197,0.0113,0.0323],-12,'#2197ff',0.5,0.7)  % Grow % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(2001,[0.0139,0.0079,0.0227],-14,'#2197ff',0.5,0.4)  % Grow % FigNo,DataSrc,yloc,MarkerColor,width,alpha

% Age 45-64
R_Plot_ErrorBar(2001,[0.0479,0.0283,0.0774],-18,'#9852d9',0.5,1)  % Aging % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(2001,[0.0667,0.0394,0.1077],-20,'#9852d9',0.5,0.7)  % Aging % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(2001,[0.0457,0.0270,0.0738],-22,'#9852d9',0.5,0.4)  % Aging % FigNo,DataSrc,yloc,MarkerColor,width,alpha

% Age ≥65
R_Plot_ErrorBar(2001,[0.1567,0.0918,0.2497],-26,'#f54e8b',0.5,1)  % Grow % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(2001,[0.1882,0.1103,0.2998],-28,'#f54e8b',0.5,0.7)  % Grow % FigNo,DataSrc,yloc,MarkerColor,width,alpha
R_Plot_ErrorBar(2001,[0.1234,0.0723,0.1966],-30,'#f54e8b',0.5,0.4)  % Grow % FigNo,DataSrc,yloc,MarkerColor,width,alpha


Plot_ErrorBar(2001,[0.0386,0.0233,0.0616],-2)
Plot_ErrorBar(2001,[0.0523,0.0315,0.0834],-4)
Plot_ErrorBar(2001,[0.0252,0.0152,0.0403],-6)

Plot_ErrorBar(2001,[0.0143,0.0082,0.0234],-10)
Plot_ErrorBar(2001,[0.0197,0.0113,0.0323],-12)
Plot_ErrorBar(2001,[0.0139,0.0079,0.0227],-14)

Plot_ErrorBar(2001,[0.0479,0.0283,0.0774],-18)
Plot_ErrorBar(2001,[0.0667,0.0394,0.1077],-20)
Plot_ErrorBar(2001,[0.0457,0.0270,0.0738],-22)


Plot_ErrorBar(2001,[0.1567,0.0918,0.2497],-26)
Plot_ErrorBar(2001,[0.1882,0.1103,0.2998],-28)
Plot_ErrorBar(2001,[0.1234,0.0723,0.1966],-30)

figure(2001)

ylim([-32 0])
xlim([0 0.3])
xlabel('Hazard-attributable health loss per 100,000 population (DALYs)');

yticks(flip(-[2,4,6,10,12,14,18,20,22,26,28,30]));

yticklabels({'TCs', 'Floods', 'Storms', ...
    'TCs', 'Floods', 'Storms', ...
    'TCs', 'Floods', 'Storms', ...
    'TCs', 'Floods', 'Storms'});


















