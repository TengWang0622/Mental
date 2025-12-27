%% ==================================

%  Developed by Teng Wang @ The University of Hong Kong
%    Contact: wang.teng19@alumni.imperial.ac.uk

%  Descriptions: Errorbar plot
%    This is designed to for post-processing visualization

%% ==================================


function R_Plot_ErrorBar(FigNo,DataSrc,yloc,MarkerColor,width,alpha,check)

% FigNo
% SrcData - varibale to present

figure(FigNo)
hold on

% 处理颜色输入 - 如果是十六进制字符串则转换为RGB
if ischar(MarkerColor) && MarkerColor(1) == '#'
    % 转换十六进制颜色为RGB
    hex_str = MarkerColor(2:end);
    MarkerColor = [hex2dec(hex_str(1:2)), hex2dec(hex_str(3:4)), hex2dec(hex_str(5:6))] / 255;
end

x1 = 0; 
x2 = DataSrc(1); 
y1 = yloc-width;
y2 = yloc+width;

% 定义矩形的四个顶点坐标（按顺序连接）
x_coords = [x1, x2, x2, x1];
y_coords = [y1, y1, y2, y2];

% 绘制填充的矩形
fill(x_coords, y_coords, MarkerColor, 'FaceAlpha', alpha, 'EdgeColor', 'none');

% 保持图形显示
hold on;

% For error bar plot

figure(FigNo)

    scatter(DataSrc(1),yloc,100,'sq','MarkerEdgeColor',MarkerColor,'MarkerFaceColor',MarkerColor)
    hold on
    scatter(DataSrc(2),yloc,30,'o','MarkerEdgeColor',MarkerColor,'MarkerFaceColor',MarkerColor)
    scatter(DataSrc(3),yloc,30,'o','MarkerEdgeColor',MarkerColor,'MarkerFaceColor',MarkerColor)
    plot([DataSrc(2),DataSrc(3)],[yloc,yloc],'color',MarkerColor,'linewidth',1)


    % Reference line
    %plot([0,0],[0,100],'--k','linewidth',0.5)


    hold on
    %xlim([0,10])
    %ylim([0,20])


    box on

    xlabel('Contribution (%)');
    %ylabel('Excess relative risk (%)');
    set(gca,'fontsize',14,'fontname','Arial')



