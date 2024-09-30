import seaborn as sns
import matplotlib.pyplot as plt
import pandas as pd

import matplotlib
from scipy.cluster.hierarchy import dendrogram, linkage

matplotlib.use('TkAgg')  # Or use 'Qt5Agg' if you prefer Qt

df = pd.read_csv('./SGItotals.csv')
df.fillna('', inplace=True)

def grouping():
    # Group by 'Phylum'
    grouped = df.groupby('Phylum').agg(list)
    return grouped

def gradient_weight(value):
    highest =  245
    lowest = 14 
    center = 100    
    # Normalize the value to a range from 0 to 1
    if value <= center:
        ratio = (value - lowest) / (center - lowest)  # From lowest to center
    else:
        ratio = (value - center) / (highest - center)  # From center to highest
    
    # Define the start (lowest), middle (white), and end (highest) colors
    color_highest = (228, 63, 111)  # #e43f6f
    color_white = (255, 255, 255)   # #FFFFFF
    color_lowest = (0, 141, 213)    # #008DD5
    
    # Calculate the interpolated color
    if value <= center:
        # Interpolating between lowest color and white
        red = int(color_lowest[0] + (color_white[0] - color_lowest[0]) * ratio)
        green = int(color_lowest[1] + (color_white[1] - color_lowest[1]) * ratio)
        blue = int(color_lowest[2] + (color_white[2] - color_lowest[2]) * ratio)
    else:
        # Interpolating between white and highest color
        red = int(color_white[0] + (color_highest[0] - color_white[0]) * ratio)
        green = int(color_white[1] + (color_highest[1] - color_white[1]) * ratio)
        blue = int(color_white[2] + (color_highest[2] - color_white[2]) * ratio)
    
    # Return the color in hex format
    return f'#{red:02X}{green:02X}{blue:02X}'

def gradient_ros(value):
    highest =  38848
    lowest = -22848 
    center = 0   
    # Normalize the value to a range from 0 to 1
    if value <= center:
        ratio = (value - lowest) / (center - lowest)  # From lowest to center
    else:
        ratio = (value - center) / (highest - center)  # From center to highest
    
    # Define the start (lowest), middle (white), and end (highest) colors
    color_lowest = (228, 63, 111)  # #e43f6f
    color_white = (255, 255, 255)   # #FFFFFF
    color_highest = (0, 141, 213)    # #008DD5
    
    # Calculate the interpolated color
    if value <= center:
        # Interpolating between lowest color and white
        red = int(color_lowest[0] + (color_white[0] - color_lowest[0]) * ratio)
        green = int(color_lowest[1] + (color_white[1] - color_lowest[1]) * ratio)
        blue = int(color_lowest[2] + (color_white[2] - color_lowest[2]) * ratio)
    else:
        # Interpolating between white and highest color
        red = int(color_white[0] + (color_highest[0] - color_white[0]) * ratio)
        green = int(color_white[1] + (color_highest[1] - color_white[1]) * ratio)
        blue = int(color_white[2] + (color_highest[2] - color_white[2]) * ratio)
    
    # Return the color in hex format
    return f'#{red:02X}{green:02X}{blue:02X}'


def color_sap(value):
    return '#008DD5' if value else '#ffffff'

def get_signifcance_outline(value):
    return 0.5 if value == 'neutral' else 2.0

def get_signifcance_zorder(value):
    return 0.5 if value == 'neutral' else 2.0

def listing(gp):
    # Prepare text for plotting
    grouped_text = []
    for phylum, row in gp.iterrows():
        grouped_text.append(phylum + ':')
        for genus, isolate in zip(row['Genus'], row['isolate']):
            grouped_text.append(f"  {genus}: {isolate}")

    # Plotting
    plt.figure(figsize=(5, 11))
    plt.axis('off')  # Turn off the axis

    # Labels
    y_pos = 0.5
    plt.text(-7, y_pos+0.2, "Phylum", ha='left', va='center', fontsize=12, fontweight='bold')
    plt.text(-4, y_pos+0.2, "Genus", ha='left', va='center', fontsize=12, fontweight='bold')
    plt.text(2, y_pos+0.2, "Isolate", ha='right', va='center', fontsize=12, fontweight='bold')
    plt.text(4.5, y_pos+0.3, "ROS", ha='right', va='center', fontsize=12, fontweight='bold',rotation=45)
    plt.text(5.125, y_pos+0.3, "SGI", ha='right', va='center', fontsize=12, fontweight='bold',rotation=45)
    plt.text(5.75, y_pos+0.3, "Sap", ha='right', va='center', fontsize=12, fontweight='bold',rotation=45)

    for i, (phylum, row) in enumerate(gp.iterrows()):
        _y_pos = len(list(zip(row['Genus'], row['isolate'], row['p_weight_change'])))
        # Horizontal line
        plt.plot([-4, 5.5], [y_pos, y_pos], color='#373F51', linewidth=0.5)  

        # Angled Phylum names
        plt.text(-4.5, y_pos-(_y_pos*0.25/2), phylum, ha='right', va='top', fontsize=12, rotation=45),
        
        for genus, isolate, p_weight, sap_positive, state, ros_live_state, ros_live_values in zip(row['Genus'], row['isolate'], row['p_weight_change'], row['sap_positive'], row['state'], row['ros_live_state'], row['ros_live_values']):
            y_pos -= 0.3

            # Genus and isolate
            plt.text(-4, y_pos, f"{genus}", ha='left', va='center', fontsize=12, fontstyle='italic')
            plt.text(3, y_pos, isolate, ha='right', va='center', fontsize=12)
            
            # Colored squares
            plt.scatter(3.6, y_pos, s=250, color=gradient_ros(ros_live_values), marker='s', edgecolor='#373F51', linewidths=get_signifcance_outline(ros_live_state), zorder=get_signifcance_zorder(ros_live_state))  # Square with size 10
            plt.scatter(4.3, y_pos, s=250, color=gradient_weight(p_weight), marker='s', edgecolor='#373F51', linewidths=get_signifcance_outline(state), zorder=get_signifcance_zorder(state))  # Square with size 10
            plt.scatter(5, y_pos, s=250, color=color_sap(sap_positive), marker='s', edgecolor='#373F51', linewidths=0.5)  # Square with size 10

        # Increase spacing after each Phylum group
        y_pos -= 0.3  
    
    # Horizontal line
    plt.plot([-4, 5.5], [y_pos-0.1, y_pos-0.1], color='#373F51', linewidth=0.5)  

    # Show the plot
    plt.savefig("thesis.png", bbox_inches='tight', dpi=300)

grouped_data = grouping()
listing(grouped_data)

# print(grouped_data.head)